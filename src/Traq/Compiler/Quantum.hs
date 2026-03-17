{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Compiler.Quantum (
  lowerProgram,

  -- * Class
  CompileQ (..),
) where

import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..))

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis.Annotate.Prelude as A
import qualified Traq.CPL as CPL
import Traq.Compiler.Prelude
import Traq.Compiler.Unitary
import Traq.Prelude
import Traq.QPL.Syntax

-- ================================================================================
-- Compiler
-- ================================================================================

class (CompileU ext) => CompileQ ext where
  compileQ ::
    forall ext' m.
    ( m ~ CompilerT ext'
    , SizeType ext ~ SizeType ext'
    , PrecType ext ~ PrecType ext'
    , CompileQ ext'
    ) =>
    ext ->
    [Ident] ->
    m (Stmt (SizeType ext))

instance (CPL.TypingReqs size, Integral size) => CompileQ (CPL.Core size prec) where
  compileQ = \case {}

instance (CPL.TypingReqs size, Integral size) => CompileQ (A.AnnFailProb (CPL.Core size prec)) where
  compileQ (A.AnnFailProb _ ext) = case ext of {}

class CompileQ1 f where
  -- | all arguments/info provided to compile the given data
  type CompileQArgs f ext

  -- | output of the compilation.
  type CompileQResult f ext

  compileQ1 ::
    forall ext m.
    ( CompileQ ext
    , CPL.TypeInferrable ext (SizeType ext)
    , m ~ CompilerT ext
    ) =>
    CompileQArgs f ext ->
    f ext ->
    m (CompileQResult f ext)

instance CompileQ1 CPL.Expr where
  type CompileQArgs CPL.Expr ext = [Ident]
  type CompileQResult CPL.Expr ext = Stmt (SizeType ext)

  -- basic expressions
  compileQ1 rets CPL.BasicExprE{CPL.basic_expr} = return $ AssignS rets basic_expr
  -- random sampling expressions
  compileQ1 rets CPL.RandomSampleE{CPL.distr_expr} = return $ RandomS rets distr_expr
  -- function call
  compileQ1 rets CPL.FunCallE{CPL.fname, CPL.args} = do
    let proc_id = mkQProcName fname
    return $ CallS{fun = FunctionCall proc_id, args = map Arg (args ++ rets), meta_params = []}
  -- primitive call
  compileQ1 rets CPL.PrimCallE{CPL.prim} = compileQ prim rets

instance CompileQ1 CPL.Stmt where
  type CompileQArgs CPL.Stmt ext = ()
  type CompileQResult CPL.Stmt ext = Stmt (SizeType ext)

  compileQ1 () CPL.ExprS{CPL.rets, CPL.expr} = compileQ1 rets expr
  compileQ1 () (CPL.SeqS ss) = SeqS <$> mapM (compileQ1 ()) ss
  compileQ1 () CPL.IfThenElseS{CPL.cond, CPL.s_true, CPL.s_false} = do
    s_true <- compileQ1 () s_true
    s_false <- compileQ1 () s_false
    pure IfThenElseS{..}
  compileQ1 () CPL.ForS{CPL.loop_ix, CPL.loop_ty, CPL.loop_body} = do
    n <- case loop_ty of
      CPL.Fin n -> pure n
      _ -> throwError "for loop index must be of type `Fin`"
    iter_meta_var <- newIdent "ITER"
    CPL._typingCtx . Ctx.ins loop_ix .= loop_ty
    loop_body <- compileQ1 () loop_body
    pure $
      ForInRangeS
        { iter_meta_var
        , iter_lim = CPL.MetaSize n
        , loop_body = SeqS [AssignS [loop_ix] (CPL.ParamE iter_meta_var), loop_body]
        }

instance CompileQ1 CPL.FunBody where
  type CompileQArgs CPL.FunBody ext = ([CPL.VarType (SizeType ext)], [CPL.VarType (SizeType ext)])
  type CompileQResult CPL.FunBody ext = CProcBody (SizeType ext)

  compileQ1 (param_types, _ret_types) CPL.FunBody{CPL.param_names, CPL.ret_names, CPL.body_stmt} = do
    CPL._typingCtx .= Ctx.fromList (zip param_names param_types)
    magnify CPL._funCtx . zoom CPL._typingCtx . ignoreWriter $ CPL.inferTypes body_stmt

    cproc_body_stmt <- compileQ1 () body_stmt
    proc_typing_ctx <- use CPL._typingCtx
    CPL._typingCtx .= mempty

    let cproc_param_names = param_names ++ ret_names
    let cproc_local_vars =
          proc_typing_ctx
            & Ctx.toList
            & filter ((`notElem` cproc_param_names) . fst)

    let cproc_body = CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt}
    return cproc_body

instance CompileQ1 CPL.FunDef where
  type CompileQArgs CPL.FunDef ext = Ident
  type CompileQResult CPL.FunDef ext = ProcDef (SizeType ext)

  -- lower declarations as-is, ignoring fail prob
  compileQ1 proc_name CPL.FunDef{CPL.param_types, CPL.ret_types, CPL.mbody = Nothing} = do
    return
      ProcDef
        { info_comment = ""
        , proc_name
        , proc_meta_params = []
        , proc_param_types = param_types ++ ret_types
        , proc_body = ProcBodyC CProcDecl
        }
  compileQ1 proc_name CPL.FunDef{CPL.param_types, CPL.ret_types, CPL.mbody = Just body} = do
    cproc_body <- compileQ1 (param_types, ret_types) body

    return
      ProcDef
        { info_comment = ""
        , proc_name
        , proc_meta_params = []
        , proc_param_types = param_types ++ ret_types
        , proc_body = ProcBodyC cproc_body
        }

instance CompileQ1 CPL.NamedFunDef where
  type CompileQArgs CPL.NamedFunDef ext = ()
  type CompileQResult CPL.NamedFunDef ext = ()

  compileQ1 () CPL.NamedFunDef{CPL.fun_name, CPL.fun_def} = do
    let proc_name = mkQProcName fun_name
    proc_def <- compileQ1 proc_name fun_def
    addProc proc_def

instance CompileQ1 CPL.Program where
  type CompileQArgs CPL.Program ext = ()
  type CompileQResult CPL.Program ext = ()

  compileQ1 () (CPL.Program fs) = forM_ fs $ \f -> do
    compileU1 () f
    compileQ1 () f

-- ================================================================================
-- Entry Point
-- ================================================================================

-- | Lower a full program into a QPL program.
lowerProgram ::
  forall ext size prec.
  ( CPL.TypingReqs size
  , Floating prec
  , CPL.HasFreeVars ext
  , CompileQ ext
  , CPL.TypeInferrable ext size
  , PrecType ext ~ prec
  , SizeType ext ~ size
  ) =>
  CPL.Program ext ->
  Either String (Program size)
lowerProgram = compileWith (compileQ1 ())
