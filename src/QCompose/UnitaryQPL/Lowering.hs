module QCompose.UnitaryQPL.Lowering where

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans (lift)
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.Mtl

import QCompose.Basic
import qualified QCompose.ProtoLang as P
import QCompose.UnitaryQPL.Syntax
import QCompose.Utils.Context
import QCompose.Utils.MonadHelpers

-- | Formulas for primitives
data QSearchUnitaryImpl a = QSearchUnitaryImpl
  { qsearchAncilla :: SizeT -> Precision -> [P.VarType a]
  }

-- | Configuration for lowering
type LoweringConfig a = (P.FunCtx a, QSearchUnitaryImpl a)

protoFunCtx :: Lens' (LoweringConfig a) (P.FunCtx a)
protoFunCtx = _1

qsearchConfig :: Lens' (LoweringConfig a) (QSearchUnitaryImpl a)
qsearchConfig = _2

{- | A global lowering context storing the source functions and generated procedures
 along with info to generate unique ancilla and variable/procedure names
-}
type LoweringCtx a = (Int, Int, P.TypingCtx a)

emptyLoweringCtx :: LoweringCtx a
emptyLoweringCtx = (0, 0, M.empty)

ancillaIdx, uniqNameIdx :: Lens' (LoweringCtx a) Int
ancillaIdx = _1
uniqNameIdx = _2

typingCtx :: Lens' (LoweringCtx a) (P.TypingCtx a)
typingCtx = _3

-- | The outputs of lowering
type LoweringOutput a = [ProcDef a]

loweredProcs :: Lens' (LoweringOutput a) [ProcDef a]
loweredProcs = id

{- | Monad to compile ProtoQB to UQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT a = RWST (LoweringConfig a) (LoweringOutput a) (LoweringCtx a) (Either String)

-- | Allocate an ancilla register, and update the typing context.
allocAncilla :: P.VarType a -> CompilerT a Ident
allocAncilla ty = do
  i <- use ancillaIdx
  ancillaIdx += 1

  let name = "_aux_" <> show i
  zoom typingCtx $ putValue name ty
  return name

-- | A simple name-mangling utility to generate a unique proc name.
newProcName :: Ident -> CompilerT a Ident
newProcName name = do
  i <- use uniqNameIdx
  uniqNameIdx += 1
  return $ name <> "_" <> show i

-- | Add a new procedure.
addProc :: ProcDef a -> CompilerT a ()
addProc procDef = tell [procDef]

lowerExpr ::
  (P.TypeCheckable a) =>
  Precision ->
  P.Expr a ->
  -- | returns
  [Ident] ->
  CompilerT a (Stmt a)
lowerExpr _ P.VarE{P.arg} [ret] = do
  ty <- zoom typingCtx $ lookupVar arg
  return $ UnitaryS [arg, ret] $ RevEmbedU $ IdF ty
-- error out in all other cases
lowerExpr _ e rs = throwError $ "cannot compile: invalid expression: " <> show (e, rs)

lowerStmt ::
  (P.TypeCheckable a) =>
  Precision ->
  P.Stmt a ->
  CompilerT a (Stmt a)
-- single statement
lowerStmt delta s@P.ExprS{P.rets, P.expr} = do
  checker <- P.checkStmt <$> view protoFunCtx <*> pure s
  zoom typingCtx $ embedStateT checker
  lowerExpr delta expr rets

-- compound statements
lowerStmt _ (P.SeqS []) = return SkipS
lowerStmt delta (P.SeqS [s]) = lowerStmt delta s
lowerStmt delta (P.SeqS (s : ss)) = do
  s' <- lowerStmt (delta / 2) s
  ss' <- lowerStmt (delta / 2) (P.SeqS ss)
  return $ SeqS [s', ss']

-- unsupported
lowerStmt _ _ = error "lowering: unsupported"

-- | A procDef generated from a funDef, along with the partitioned register spaces.
data LoweredProc a = LoweredProc
  { lowered_def :: ProcDef a
  , -- | the inputs to the original fun
    inp_params :: P.TypingCtx a
  , -- | the outputs of the original fun
    out_params :: P.TypingCtx a
  , -- | all other registers
    aux_params :: P.TypingCtx a
  }

lowerFunDef ::
  (P.TypeCheckable a) =>
  Precision ->
  P.FunDef a ->
  CompilerT a (LoweredProc a)
lowerFunDef = error "TODO lowerFunDef"

-- | Lower a full program into a UQPL program.
lowerProgram ::
  (P.TypeCheckable a) =>
  QSearchUnitaryImpl a ->
  P.TypingCtx a ->
  Precision ->
  P.Program a ->
  Either String (Program a)
lowerProgram qsearch_config gamma_in delta P.Program{P.funCtx, P.stmt} = do
  (stmtU, _, outputU) <- runRWST compiler config ctx
  return
    Program
      { oracle_decl = funCtx & P.oracle_decl
      , proc_defs = outputU ^. loweredProcs
      , stmt = stmtU
      }
 where
  config = (funCtx, qsearch_config)
  ctx = emptyLoweringCtx & typingCtx .~ gamma_in
  compiler = lowerStmt delta stmt
