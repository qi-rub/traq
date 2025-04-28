{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.CQPL.Lowering (
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

import Control.Monad (msum, unless)
import Control.Monad.Except (throwError)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.CQPL.Syntax
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

-- | Configuration for lowering
type LoweringEnv primT holeT sizeT costT = (P.FunCtx primT sizeT, P.OracleName)

protoFunCtx :: Lens' (LoweringEnv primT holeT sizeT costT) (P.FunCtx primT sizeT)
protoFunCtx = _1

oracleName :: Lens' (LoweringEnv primT holeT sizeT costT) P.OracleName
oracleName = _2

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
type LoweringOutput holeT sizeT = ([ProcDef holeT sizeT], [UQPL.ProcDef holeT sizeT])

loweredProcs :: Lens' (LoweringOutput holeT sizeT) [ProcDef holeT sizeT]
loweredProcs = _1

loweredUProcs :: Lens' (LoweringOutput holeT sizeT) [UQPL.ProcDef holeT sizeT]
loweredUProcs = _2

{- | Monad to compile ProtoQB to CQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT primT holeT sizeT costT = MyReaderWriterStateT (LoweringEnv primT holeT sizeT costT) (LoweringOutput holeT sizeT) (LoweringCtx sizeT) (Either String)

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

instance Lowerable primsT Void holeT sizeT costT where
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
addProc :: ProcDef holeT sizeT -> CompilerT primT holeT sizeT costT ()
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
  -- | source function
  P.FunDef primsT sizeT ->
  CompilerT primsT holeT sizeT costT Ident
-- lower declarations as-is, ignoring fail prob
lowerFunDef _ P.FunDef{P.fun_name, P.param_types, P.ret_types, P.mbody = Nothing} = do
  is_oracle <- (fun_name ==) <$> view oracleName
  let proc_def =
        ProcDef
          { proc_name = fun_name
          , proc_meta_params = []
          , proc_param_types = param_types ++ ret_types
          , mproc_body = Nothing
          , is_oracle
          }
  addProc proc_def
  return fun_name
lowerFunDef eps P.FunDef{P.fun_name, P.param_types, P.mbody = Just body} = do
  is_oracle <- (fun_name ==) <$> view oracleName

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
      , mproc_body = Just ProcBody{proc_param_names, proc_local_vars, proc_body_stmt}
      , is_oracle
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
  fun_def <- view $ protoFunCtx . Ctx.at f . singular _Just
  lowerFunDef eps fun_def

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
lowerExpr _ P.VarE{P.arg} [ret] = return $ AssignS [ret] VarE{var = arg}
lowerExpr _ P.ConstE{P.val, P.ty} [ret] = return $ AssignS [ret] ConstE{val, val_ty = ty}
lowerExpr _ P.UnOpE{P.un_op, P.arg} [ret] =
  let expr = case un_op of
        P.NotOp -> NotE (VarE arg)
   in return $ AssignS{rets = [ret], expr}
lowerExpr eps P.FunCallE{P.fun_kind = P.FunctionCall f, P.args} rets = do
  proc_name <- lowerFunDefByName eps f
  return $ CallS{fun = FunctionCall proc_name, args = args ++ rets, meta_params = []}
lowerExpr eps P.FunCallE{P.fun_kind = P.PrimitiveCall prim, P.args} rets =
  lowerPrimitive eps prim args rets
lowerExpr _ _ _ = throwError "lowering: unsupported"

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
  censored . magnify protoFunCtx . zoom typingCtx $ P.checkStmt s
  lowerExpr eps expr rets

-- compound statements
lowerStmt _ (P.SeqS []) = return SkipS
lowerStmt eps (P.SeqS [s]) = lowerStmt eps s
lowerStmt eps (P.SeqS (s : ss)) = do
  s' <- lowerStmt (eps / 2) s
  ss' <- lowerStmt (eps / 2) (P.SeqS ss)
  return $ SeqS [s', ss']

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
  -- | input bindings to the source program
  P.TypingCtx sizeT ->
  -- | name of the oracle
  P.OracleName ->
  -- | fail prob \( \varepsilon \)
  costT ->
  -- | source program
  P.Program primsT sizeT ->
  Either String (Program holeT sizeT, P.TypingCtx sizeT)
lowerProgram gamma_in oracle_name eps prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config = (funCtx, oracle_name)
  let lowering_ctx =
        emptyLoweringCtx
          & typingCtx .~ gamma_in
          & uniqNames .~ P.allNamesP prog

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
