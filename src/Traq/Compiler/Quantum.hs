{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Compiler.Quantum (
  -- * Compilation
  lowerProgram,

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
-- Compilation
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
lowerExpr P.RandomSampleE{P.distr_expr = P.UniformE{}} _rets = do
  error "TODO uniform random sampling operation"
lowerExpr P.RandomSampleE{P.distr_expr = P.BernoulliE{}} _rets = do
  error "TODO biased coin toss"

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
