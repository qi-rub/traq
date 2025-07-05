{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Lowering (
  -- * Compilation
  lowerProgram,
  newIdent,
  addProc,

  -- * Types
  CompilerT,
  LoweringEnv,
  LoweringCtx,
  LoweringOutput,

  -- * Lenses
  loweredProcs,
  loweredUProcs,
  typingCtx,

  -- * Primitive implementations
  Lowerable (..),
) where

import Control.Monad (msum, unless, zipWithM)
import Control.Monad.Except (throwError)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import Traq.CQPL.Syntax
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL

-- | Configuration for lowering
type LoweringEnv primT sizeT costT = P.QuantumMaxCostEnv primT sizeT costT

{- | A global lowering context storing the source functions and generated procedures
 along with info to generate unique ancilla and variable/procedure names
-}
type LoweringCtx sizeT = (Set.Set Ident, P.TypingCtx sizeT)

emptyLoweringCtx :: LoweringCtx sizeT
emptyLoweringCtx = (Set.empty, Ctx.empty)

uniqNames :: Lens' (LoweringCtx sizeT) (Set.Set Ident)
uniqNames = _1

typingCtx :: Lens' (LoweringCtx sizeT) (P.TypingCtx sizeT)
typingCtx = _2

-- | The outputs of lowering
type LoweringOutput holeT sizeT costT = ([ProcDef holeT sizeT costT], [UQPL.ProcDef holeT sizeT costT])

loweredProcs :: Lens' (LoweringOutput holeT sizeT costT) [ProcDef holeT sizeT costT]
loweredProcs = _1

loweredUProcs :: Lens' (LoweringOutput holeT sizeT costT) [UQPL.ProcDef holeT sizeT costT]
loweredUProcs = _2

{- | Monad to compile ProtoQB to CQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT primT holeT sizeT costT = MyReaderWriterStateT (LoweringEnv primT sizeT costT) (LoweringOutput holeT sizeT costT) (LoweringCtx sizeT) (Either String)

-- | Primitives that support a classical-quantum lowering.
class
  (UQPL.Lowerable primsT primT holeT sizeT costT) =>
  Lowerable primsT primT holeT sizeT costT
  where
  lowerPrimitive ::
    -- | fail prob
    costT ->
    primT ->
    -- | args
    [Ident] ->
    -- | rets
    [Ident] ->
    CompilerT primsT holeT sizeT costT (Stmt holeT sizeT)

instance (Show costT) => Lowerable primsT Void holeT sizeT costT where
  lowerPrimitive _ = absurd

-- | Generate a new identifier with the given prefix.
newIdent :: forall primT holeT sizeT costT. Ident -> CompilerT primT holeT sizeT costT Ident
newIdent prefix = do
  ident <-
    msum . map checked $
      prefix : map ((prefix <>) . ("_" <>) . show) [1 :: Int ..]
  uniqNames . at ident ?= ()
  return ident
 where
  checked :: Ident -> CompilerT primT holeT sizeT costT Ident
  checked name = do
    already_exists <- use (uniqNames . at name)
    case already_exists of
      Nothing -> return name
      Just () -> throwError "next ident please!"

-- | Add a new procedure.
addProc :: ProcDef holeT sizeT costT -> CompilerT primT holeT sizeT costT ()
addProc = writeElemAt loweredProcs

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
  let tick = error "TODO pass tick mapping to CQPL compiler"
  let proc_def =
        ProcDef
          { proc_name = fun_name
          , proc_meta_params = []
          , proc_param_types = param_types ++ ret_types
          , proc_body_or_tick = Left tick
          }
  addProc proc_def
  return fun_name
lowerFunDef eps fun_name P.FunDef{P.param_types, P.mbody = Just body} = do
  proc_name <- newIdent $ printf "%s[%s]" fun_name (show eps)

  let P.FunBody{P.param_names, P.ret_names, P.body_stmt} = body

  (proc_body_stmt, proc_typing_ctx) <- withSandbox $ do
    typingCtx .= Ctx.fromList (zip param_names param_types)
    b <- lowerStmt eps body_stmt
    c <- use typingCtx
    return (b, c)

  let proc_param_names = param_names ++ ret_names
  let proc_local_vars =
        proc_typing_ctx
          & Ctx.toList
          & filter ((`notElem` proc_param_names) . fst)

  addProc
    ProcDef
      { proc_name
      , proc_meta_params = []
      , proc_param_types = map (\x -> proc_typing_ctx ^?! Ctx.at x . _Just) proc_param_names
      , proc_body_or_tick = Right ProcBody{proc_param_names, proc_local_vars, proc_body_stmt}
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
lowerExpr _ P.BasicExprE{P.basic_expr} rets = return $ AssignS rets basic_expr
lowerExpr eps P.FunCallE{P.fun_kind = P.FunctionCall f, P.args} rets = do
  proc_name <- lowerFunDefByName eps f
  return $ CallS{fun = FunctionCall proc_name, args = args ++ rets, meta_params = []}
lowerExpr eps P.FunCallE{P.fun_kind = P.PrimitiveCall prim, P.args} rets =
  lowerPrimitive eps prim args rets

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
  censored . magnify P._funCtx . zoom typingCtx $ P.typeCheckStmt s
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
  Either String (Program holeT sizeT costT, P.TypingCtx sizeT)
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
        emptyLoweringCtx
          & (typingCtx .~ gamma_in)
          & (uniqNames .~ P.allNamesP prog)

  let compiler = lowerStmt eps stmt
  (stmtQ, lowering_ctx', outputU) <- runMyReaderWriterStateT compiler config lowering_ctx

  return
    ( Program
        { proc_defs = outputU ^. loweredProcs . to (Ctx.fromListWith proc_name)
        , uproc_defs = outputU ^. loweredUProcs . to (Ctx.fromListWith UQPL.proc_name)
        , stmt = stmtQ
        }
    , lowering_ctx' ^. typingCtx
    )
