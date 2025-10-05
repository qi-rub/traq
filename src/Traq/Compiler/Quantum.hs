{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import Control.Monad (unless, zipWithM)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Void (Void, absurd)
import GHC.Generics hiding (to)
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

-- | Configuration for lowering
type LoweringEnv primT sizeT precT = P.QuantumMaxCostEnv primT sizeT precT

{- | Monad to compile ProtoQB to CQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT primT sizeT precT =
  WriterT
    (LoweringOutput sizeT)
    ( ReaderT
        (LoweringEnv primT sizeT precT)
        ( StateT
            (LoweringCtx sizeT)
            (Either String)
        )
    )

-- | Primitives that support a classical-quantum lowering.
class
  (CompileU.Lowerable primsT primT sizeT precT) =>
  Lowerable primsT primT sizeT precT
  where
  lowerPrimitive ::
    -- | fail prob
    precT ->
    primT ->
    -- | rets
    [Ident] ->
    CompilerT primsT sizeT precT (Stmt sizeT)

instance (Show precT) => Lowerable primsT Void sizeT precT where
  lowerPrimitive _ = absurd

-- | Generic
class GLowerable primsT f sizeT precT where
  glowerPrimitive ::
    f primT ->
    precT ->
    -- | rets
    [Ident] ->
    CompilerT primsT sizeT precT (Stmt sizeT)

instance
  (GLowerable primsT f1 sizeT precT, GLowerable primsT f2 sizeT precT) =>
  GLowerable primsT (f1 :+: f2) sizeT precT
  where
  glowerPrimitive (L1 p) = glowerPrimitive p
  glowerPrimitive (R1 p) = glowerPrimitive p

instance
  (GLowerable primsT f sizeT precT) =>
  GLowerable primsT (M1 i c f) sizeT precT
  where
  glowerPrimitive (M1 x) = glowerPrimitive x

instance
  (Lowerable primsT f sizeT precT) =>
  GLowerable primsT (K1 i f) sizeT precT
  where
  glowerPrimitive (K1 x) delta = lowerPrimitive delta x

-- ================================================================================
-- Compilation
-- ================================================================================

-- | Lower a source function to a procedure call.
lowerFunDef ::
  forall primsT sizeT precT.
  ( Lowerable primsT primsT sizeT precT
  , P.TypeCheckable sizeT
  , Show precT
  , Floating precT
  ) =>
  -- | fail prob
  precT ->
  -- | source function name
  Ident ->
  -- | source function
  P.FunDef primsT sizeT ->
  CompilerT primsT sizeT precT Ident
-- lower declarations as-is, ignoring fail prob
lowerFunDef _ fun_name P.FunDef{P.param_types, P.ret_types, P.mbody = Nothing} = do
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
lowerFunDef eps fun_name P.FunDef{P.param_types, P.mbody = Just body} = do
  let info_comment = printf "%s[%s]" fun_name (show eps)
  proc_name <- newIdent fun_name

  let P.FunBody{P.param_names, P.ret_names, P.body_stmt} = body

  (cproc_body_stmt, proc_typing_ctx) <- withSandbox $ do
    P._typingCtx .= Ctx.fromList (zip param_names param_types)
    b <- lowerStmt eps body_stmt
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
  forall primsT sizeT precT.
  ( Lowerable primsT primsT sizeT precT
  , P.TypeCheckable sizeT
  , Show precT
  , Floating precT
  ) =>
  -- | fail prob
  precT ->
  -- | source function name
  Ident ->
  CompilerT primsT sizeT precT Ident
lowerFunDefByName eps f = do
  fun_def <- view $ P._funCtx . Ctx.at f . singular _Just
  lowerFunDef eps f fun_def

-- | Lower a source expression to a statement.
lowerExpr ::
  forall primsT sizeT precT.
  ( Lowerable primsT primsT sizeT precT
  , P.TypeCheckable sizeT
  , Show precT
  , Floating precT
  ) =>
  -- fail prob
  precT ->
  -- source expression
  P.Expr primsT sizeT ->
  -- return variables
  [Ident] ->
  CompilerT primsT sizeT precT (Stmt sizeT)
-- basic expressions
lowerExpr _ P.BasicExprE{P.basic_expr} rets = return $ AssignS rets basic_expr
-- random sampling expressions
lowerExpr _ P.RandomSampleE{P.distr_expr = P.UniformE{}} rets = do
  error "TODO uniform random sampling operation"
lowerExpr _ P.RandomSampleE{P.distr_expr = P.BernoulliE{P.prob_one}} rets = do
  error "TODO biased coin toss"

-- function call
lowerExpr eps P.FunCallE{P.fname, P.args} rets = do
  proc_name <- lowerFunDefByName eps fname
  return $ CallS{fun = FunctionCall proc_name, args = args ++ rets, meta_params = []}

-- primitive call
lowerExpr eps P.PrimCallE{P.prim} rets =
  lowerPrimitive eps prim rets

-- | Lower a single statement
lowerStmt ::
  forall primsT sizeT precT.
  ( Lowerable primsT primsT sizeT precT
  , P.TypeCheckable sizeT
  , Show precT
  , Floating precT
  ) =>
  precT ->
  P.Stmt primsT sizeT ->
  CompilerT primsT sizeT precT (Stmt sizeT)
-- single statement
lowerStmt eps s@P.ExprS{P.rets, P.expr} = do
  lift . magnify P._funCtx . zoom P._typingCtx $ P.typeCheckStmt s
  lowerExpr eps expr rets

-- compound statements
lowerStmt eps (P.SeqS ss) = do
  epss <- P.splitEps eps ss
  SeqS <$> zipWithM lowerStmt epss ss

-- unsupported
lowerStmt _ _ = throwError "lowering: unsupported"

-- | Lower a full program into a CQPL program.
lowerProgram ::
  forall primsT sizeT precT.
  ( Lowerable primsT primsT sizeT precT
  , P.TypeCheckable sizeT
  , Show precT
  , Floating precT
  , P.HasFreeVars primsT
  ) =>
  P.PrecisionSplittingStrategy ->
  -- | input bindings to the source program
  P.TypingCtx sizeT ->
  -- | fail prob \( \varepsilon \)
  precT ->
  -- | source program
  P.Program primsT sizeT ->
  Either String (Program sizeT)
lowerProgram strat gamma_in eps prog@(P.Program fs) = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        default_
          & (P._funCtx .~ P.namedFunsToFunCtx fs)
          & (P._precSplitStrat .~ strat)
  let lowering_ctx =
        default_
          & (P._typingCtx .~ gamma_in)
          & (_uniqNamesCtx .~ P.allNamesP prog)

  let P.NamedFunDef{P.fun_name = main_name} = last fs
  output <-
    lowerFunDefByName eps main_name
      & execWriterT
      & (runReaderT ?? config)
      & (evalStateT ?? lowering_ctx)

  return Program{proc_defs = output ^. to (Ctx.fromListWith proc_name)}
