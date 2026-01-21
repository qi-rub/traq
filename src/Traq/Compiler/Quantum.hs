{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Compiler.Quantum (
  lowerProgram,
  CompileQ (..),
  CompileQ1 (..),
) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (RWST (..))

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.CQPL.Syntax
import Traq.Compiler.Prelude
import Traq.Compiler.Unitary (CompileU, compileU1)
import Traq.Prelude
import qualified Traq.ProtoLang as P

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

instance (P.TypingReqs size) => CompileQ (P.Core size prec) where
  compileQ = \case {}

class CompileQ1 f where
  -- | all arguments/info provided to compile the given data
  type CompileQArgs f ext

  -- | output of the compilation.
  type CompileQResult f ext

  compileQ1 ::
    forall ext m.
    ( CompileQ ext
    , P.TypeInferrable ext (SizeType ext)
    , m ~ CompilerT ext
    ) =>
    CompileQArgs f ext ->
    f ext ->
    m (CompileQResult f ext)

instance CompileQ1 P.Expr where
  type CompileQArgs P.Expr ext = [Ident]
  type CompileQResult P.Expr ext = Stmt (SizeType ext)

  -- basic expressions
  compileQ1 rets P.BasicExprE{P.basic_expr} = return $ AssignS rets basic_expr
  -- random sampling expressions
  compileQ1 rets P.RandomSampleE{P.distr_expr} = return $ RandomS rets distr_expr
  -- function call
  compileQ1 rets P.FunCallE{P.fname, P.args} = do
    fun_def <- P.lookupFunE fname
    proc_def <- compileQ1 fname fun_def
    addProc proc_def
    return $ CallS{fun = FunctionCall (proc_name proc_def), args = args ++ rets, meta_params = []}
  -- primitive call
  compileQ1 rets P.PrimCallE{P.prim} = compileQ prim rets
  compileQ1 _ _ = error "TODO: UNSUPPORTED"

instance CompileQ1 P.Stmt where
  type CompileQArgs P.Stmt ext = ()
  type CompileQResult P.Stmt ext = Stmt (SizeType ext)

  compileQ1 () P.ExprS{P.rets, P.expr} = compileQ1 rets expr
  compileQ1 () (P.SeqS ss) = SeqS <$> mapM (compileQ1 ()) ss
  compileQ1 () P.IfThenElseS{P.cond, P.s_true, P.s_false} = do
    s_true <- compileQ1 () s_true
    s_false <- compileQ1 () s_false
    pure IfThenElseS{..}

instance CompileQ1 P.FunBody where
  type CompileQArgs P.FunBody ext = [P.VarType (SizeType ext)]
  type CompileQResult P.FunBody ext = (CProcBody (SizeType ext), P.TypingCtx (SizeType ext))

  compileQ1 param_types P.FunBody{P.param_names, P.ret_names, P.body_stmt} = do
    P._typingCtx .= Ctx.fromList (zip param_names param_types)
    magnify P._funCtx . zoom P._typingCtx . ignoreWriter $ P.inferTypes body_stmt

    cproc_body_stmt <- compileQ1 () body_stmt
    proc_typing_ctx <- use P._typingCtx

    let cproc_param_names = param_names ++ ret_names
    let cproc_local_vars =
          proc_typing_ctx
            & Ctx.toList
            & filter ((`notElem` cproc_param_names) . fst)

    let cproc_body = CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt}
    return (cproc_body, proc_typing_ctx)

instance CompileQ1 P.FunDef where
  type CompileQArgs P.FunDef ext = Ident
  type CompileQResult P.FunDef ext = ProcDef (SizeType ext)

  -- lower declarations as-is, ignoring fail prob
  compileQ1 proc_name P.FunDef{P.param_types, P.ret_types, P.mbody = Nothing} = do
    return
      ProcDef
        { info_comment = ""
        , proc_name
        , proc_meta_params = []
        , proc_param_types = param_types ++ ret_types
        , proc_body = ProcBodyC CProcDecl
        }
  compileQ1 proc_name P.FunDef{P.param_types, P.mbody = Just body} = do
    (cproc_body, proc_typing_ctx) <- withSandbox $ compileQ1 param_types body

    let P.FunBody{P.param_names, P.ret_names} = body
    let cproc_param_names = param_names ++ ret_names

    return
      ProcDef
        { info_comment = ""
        , proc_name
        , proc_meta_params = []
        , proc_param_types = map (\x -> proc_typing_ctx ^?! Ctx.at x . _Just) cproc_param_names
        , proc_body = ProcBodyC cproc_body
        }

instance CompileQ1 P.NamedFunDef where
  type CompileQArgs P.NamedFunDef ext = ()
  type CompileQResult P.NamedFunDef ext = ()

  compileQ1 () fn@P.NamedFunDef{P.fun_name, P.fun_def} = do
    compileU1 () fn
    let proc_name = mkQProcName fun_name
    proc_def <- compileQ1 proc_name fun_def
    addProc proc_def

instance CompileQ1 P.Program where
  type CompileQArgs P.Program ext = ()
  type CompileQResult P.Program ext = ()

  compileQ1 () (P.Program fs) = mapM_ (compileQ1 ()) fs

-- ================================================================================
-- Entry Point
-- ================================================================================

-- | Lower a full program into a CQPL program.
lowerProgram ::
  forall ext size prec.
  ( P.TypingReqs size
  , Floating prec
  , P.HasFreeVars ext
  , CompileQ ext
  , P.TypeInferrable ext size
  , PrecType ext ~ prec
  , SizeType ext ~ size
  ) =>
  P.Program ext ->
  Either String (Program size)
lowerProgram prog = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        default_
          & (P._funCtx .~ P.programToFunCtx prog)
  let lowering_ctx =
        default_
          & (_uniqNamesCtx .~ P.allNamesP prog)

  (_, _, output) <-
    compileQ1 () prog
      & (\m -> runRWST m config lowering_ctx)

  return $ Program $ output ^. _loweredProcs
