{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Compiler.Quantum (
  -- * Compilation
  lowerProgram,
  CompileQ (..),
  CompileQ1 (..),

  -- * Types
  CompilerT,
  LoweringEnv,
  LoweringCtx,
  LoweringOutput,

  -- * Primitive implementations
  Lowerable (..),
) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (RWST (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.CQPL.Syntax
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Compiler.Utils
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Primitives that support a classical-quantum lowering.
class
  (CompileU.Lowerable ext sizeT precT) =>
  Lowerable ext sizeT precT
    | ext -> sizeT precT
  where
  lowerPrimitive ::
    forall ext' m.
    ( Lowerable ext' sizeT precT
    , m ~ CompilerT ext'
    , SizeType ext' ~ sizeT
    , PrecType ext' ~ precT
    ) =>
    ext ->
    -- | rets
    [Ident] ->
    m (Stmt sizeT)

instance (P.TypingReqs sizeT) => Lowerable (P.Core sizeT precT) sizeT precT where
  lowerPrimitive = \case {}

-- ================================================================================
-- Compiler
-- ================================================================================

class CompileQ ext where
  compileQ ::
    forall ext' m.
    (m ~ CompilerT ext') =>
    ext ->
    [Ident] ->
    m (Stmt (SizeType ext))

instance CompileQ (P.Core size prec) where
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

  compileQ1 () P.NamedFunDef{P.fun_name, P.fun_def} = do
    let proc_name = mkQProcName fun_name
    proc_def <- compileQ1 proc_name fun_def
    addProc proc_def

instance CompileQ1 P.Program where
  type CompileQArgs P.Program ext = ()
  type CompileQResult P.Program ext = ()

  compileQ1 () (P.Program fs) = mapM_ (compileQ1 ()) fs

-- ================================================================================
-- Compilation (Legacy Functions)
-- ================================================================================

-- | Lower a source function to a procedure call.
lowerFunDef ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  ) =>
  -- | source function name
  Ident ->
  -- | source function
  P.FunDef ext ->
  CompilerT ext Ident
-- lower declarations as-is, ignoring fail prob
lowerFunDef fun_name P.FunDef{P.param_types, P.ret_types, P.mbody = Nothing} = do
  let proc_def =
        ProcDef
          { info_comment = ""
          , proc_name = fun_name
          , proc_meta_params = []
          , proc_param_types = param_types ++ ret_types
          , proc_body = ProcBodyC CProcDecl
          }
  addProc proc_def
  return fun_name
lowerFunDef fun_name P.FunDef{P.param_types, P.mbody = Just body} = do
  let info_comment = printf "%s" fun_name
  proc_name <- newIdent fun_name

  let P.FunBody{P.param_names, P.ret_names, P.body_stmt} = body

  (cproc_body_stmt, proc_typing_ctx) <- withSandbox $ do
    P._typingCtx .= Ctx.fromList (zip param_names param_types)
    b <- lowerStmt body_stmt
    c <- use P._typingCtx
    return (b, c)

  let cproc_param_names = param_names ++ ret_names
  let cproc_local_vars =
        proc_typing_ctx
          & Ctx.toList
          & filter ((`notElem` cproc_param_names) . fst)

  addProc
    ProcDef
      { info_comment
      , proc_name
      , proc_meta_params = []
      , proc_param_types = map (\x -> proc_typing_ctx ^?! Ctx.at x . _Just) cproc_param_names
      , proc_body = ProcBodyC $ CProcBody{cproc_param_names, cproc_local_vars, cproc_body_stmt}
      }
  return proc_name

-- | Lookup a source function by name, and lower it to a procedure call.
lowerFunDefByName ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  ) =>
  -- | source function name
  Ident ->
  CompilerT ext Ident
lowerFunDefByName f = do
  fun_def <- view $ P._funCtx . Ctx.at f . singular _Just
  lowerFunDef f fun_def

-- | Lower a source expression to a statement.
lowerExpr ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  ) =>
  -- source expression
  P.Expr ext ->
  -- return variables
  [Ident] ->
  CompilerT ext (Stmt sizeT)
-- basic expressions
lowerExpr P.BasicExprE{P.basic_expr} rets = return $ AssignS rets basic_expr
-- random sampling expressions
lowerExpr P.RandomSampleE{P.distr_expr} rets = return $ RandomS rets distr_expr
-- function call
lowerExpr P.FunCallE{P.fname, P.args} rets = do
  proc_name <- lowerFunDefByName fname
  return $ CallS{fun = FunctionCall proc_name, args = args ++ rets, meta_params = []}

-- primitive call
lowerExpr P.PrimCallE{P.prim} rets =
  lowerPrimitive prim rets
lowerExpr _ _ = error "TODO: UNSUPPORTED"

-- | Lower a single statement
lowerStmt ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  ) =>
  P.Stmt ext ->
  CompilerT ext (Stmt sizeT)
-- single statement
lowerStmt s@P.ExprS{P.rets, P.expr} = do
  _ <- ignoreWriter . magnify P._funCtx . zoom P._typingCtx $ P.inferTypes s
  lowerExpr expr rets

-- compound statements
lowerStmt (P.SeqS ss) = SeqS <$> mapM lowerStmt ss
-- unsupported
lowerStmt _ = throwError "lowering: unsupported"

-- | Lower a full program into a CQPL program.
lowerProgram ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  , P.HasFreeVars ext
  ) =>
  P.Program ext ->
  Either String (Program sizeT)
lowerProgram prog@(P.Program fs) = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        default_
          & (P._funCtx .~ P.namedFunsToFunCtx fs)
  let lowering_ctx =
        default_
          -- & (P._typingCtx .~ gamma_in)
          & (_uniqNamesCtx .~ P.allNamesP prog)

  let P.NamedFunDef{P.fun_name = main_name} = last fs

  (_, _, output) <-
    lowerFunDefByName main_name
      & (\m -> runRWST m config lowering_ctx)

  return $ Program $ output ^. _loweredProcs
