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
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, runWriterT)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.CQPL.Syntax
import qualified Traq.Compiler.Unitary as CompileU
import Traq.Compiler.Utils
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Configuration for lowering
type LoweringEnv primT sizeT costT = P.QuantumMaxCostEnv primT sizeT costT

{- | Monad to compile ProtoQB to CQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT primT holeT sizeT costT =
  WriterT
    (LoweringOutput holeT sizeT costT)
    ( ReaderT
        (LoweringEnv primT sizeT costT)
        ( StateT
            (LoweringCtx sizeT)
            (Either String)
        )
    )

-- | Primitives that support a classical-quantum lowering.
class
  (CompileU.Lowerable primsT primT holeT sizeT costT) =>
  Lowerable primsT primT holeT sizeT costT
  where
  lowerPrimitive ::
    -- | fail prob
    costT ->
    primT ->
    -- | rets
    [Ident] ->
    CompilerT primsT holeT sizeT costT (Stmt holeT sizeT)

instance (Show costT) => Lowerable primsT Void holeT sizeT costT where
  lowerPrimitive _ = absurd

-- ================================================================================
-- Compilation
-- ================================================================================

-- | Lower a source function to a procedure call.
lowerFunDef ::
  forall primsT sizeT costT holeT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  -- | fail prob
  costT ->
  -- | source function name
  Ident ->
  -- | source function
  P.FunDef primsT sizeT ->
  CompilerT primsT holeT sizeT costT Ident
-- lower declarations as-is, ignoring fail prob
lowerFunDef _ fun_name P.FunDef{P.param_types, P.ret_types, P.mbody = Nothing} = do
  tick <- view $ P._classicalTicks . at fun_name . to (fromMaybe 0)
  let proc_def =
        ProcDef
          { info_comment = ""
          , proc_name = fun_name
          , proc_meta_params = []
          , proc_param_types = param_types ++ ret_types
          , proc_body = ProcBodyC $ CProcDecl tick
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
  forall primsT sizeT costT holeT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  -- | fail prob
  costT ->
  -- | source function name
  Ident ->
  CompilerT primsT holeT sizeT costT Ident
lowerFunDefByName eps f = do
  fun_def <- view $ P._funCtx . Ctx.at f . singular _Just
  lowerFunDef eps f fun_def

-- | Lower a source expression to a statement.
lowerExpr ::
  forall primsT sizeT costT holeT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  -- fail prob
  costT ->
  -- source expression
  P.Expr primsT sizeT ->
  -- return variables
  [Ident] ->
  CompilerT primsT holeT sizeT costT (Stmt holeT sizeT)
-- basic expressions
lowerExpr _ P.BasicExprE{P.basic_expr} rets = return $ AssignS rets basic_expr
-- random sampling expressions
lowerExpr _ P.UniformRandomE{} _ = error "TODO uniform random sampling operation"
lowerExpr _ P.BiasedCoinE{} _ = error "TODO biased coin toss"
-- function call
lowerExpr eps P.FunCallE{P.fname, P.args} rets = do
  proc_name <- lowerFunDefByName eps fname
  return $ CallS{fun = FunctionCall proc_name, args = args ++ rets, meta_params = []}

-- primitive call
lowerExpr eps P.PrimCallE{P.prim} rets =
  lowerPrimitive eps prim rets

-- | Lower a single statement
lowerStmt ::
  forall primsT sizeT costT holeT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  costT ->
  P.Stmt primsT sizeT ->
  CompilerT primsT holeT sizeT costT (Stmt holeT sizeT)
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
  forall primsT holeT sizeT costT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  , P.HasFreeVars primsT
  ) =>
  P.PrecisionSplittingStrategy ->
  -- | input bindings to the source program
  P.TypingCtx sizeT ->
  -- | unitary ticks
  P.OracleTicks costT ->
  -- | classical ticks
  P.OracleTicks costT ->
  -- | fail prob \( \varepsilon \)
  costT ->
  -- | source program
  P.Program primsT sizeT ->
  Either String (Program holeT sizeT costT)
lowerProgram strat gamma_in uticks cticks eps prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        default_
          & (P._funCtx .~ funCtx)
          & (P._unitaryTicks .~ uticks)
          & (P._classicalTicks .~ cticks)
          & (P._precSplitStrat .~ strat)
  let lowering_ctx =
        default_
          & (P._typingCtx .~ gamma_in)
          & (_uniqNamesCtx .~ P.allNamesP prog)

  ((stmtQ, outputU), lowering_ctx') <-
    lowerStmt eps stmt
      & runWriterT
      & (runReaderT ?? config)
      & (runStateT ?? lowering_ctx)

  let main_proc_vars = lowering_ctx' ^. P._typingCtx . to Ctx.toList
  let main_proc =
        ProcDef
          { info_comment = ""
          , proc_name = "main"
          , proc_meta_params = []
          , proc_param_types = map snd main_proc_vars
          , proc_body =
              ProcBodyC $
                CProcBody
                  { cproc_param_names = map fst main_proc_vars
                  , cproc_local_vars = []
                  , cproc_body_stmt = stmtQ
                  }
          }

  return
    Program
      { proc_defs = (outputU ^. to (Ctx.fromListWith proc_name)) & Ctx.ins "main" .~ main_proc
      }
