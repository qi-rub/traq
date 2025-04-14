module QCompose.CQPL.Lowering (
  -- * Compilation
  lowerProgram,

  -- * Primitive implementations
  QSearchCQImpl (..),
  QSearchAlgorithm,
) where

import Control.Monad (forM, msum, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.CQPL.Syntax
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

-- | Shape of a QSearch implementation
type QSearchAlgorithm holeT sizeT costT =
  -- | search elem type
  P.VarType sizeT ->
  -- | number of classical samples
  sizeT ->
  -- | max fail prob
  costT ->
  -- | unitary predicate caller
  (Ident -> Ident -> UQPL.Stmt holeT sizeT) ->
  -- | cqpl predicate caller
  (Ident -> Ident -> Stmt sizeT) ->
  -- | arguments to the function
  [(Ident, P.VarType sizeT)] ->
  -- | the generated QSearch procedure
  ProcDef sizeT

-- | Formulas for primitives
data QSearchCQImpl holeT sizeT costT = QSearchCQImpl
  { costFormulas :: P.QSearchFormulas sizeT costT
  , qsearchAlgo :: QSearchAlgorithm holeT sizeT costT
  , unitaryImpl :: UQPL.QSearchUnitaryImpl holeT sizeT costT
  }

-- | Configuration for lowering
type LoweringEnv holeT sizeT costT = (P.FunCtx sizeT, P.OracleName, QSearchCQImpl holeT sizeT costT)

protoFunCtx :: Lens' (LoweringEnv holeT sizeT costT) (P.FunCtx sizeT)
protoFunCtx = _1

oracleName :: Lens' (LoweringEnv holeT sizeT costT) P.OracleName
oracleName = _2

qsearchConfig :: Lens' (LoweringEnv holeT sizeT costT) (QSearchCQImpl holeT sizeT costT)
qsearchConfig = _3

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
type LoweringOutput holeT sizeT = ([ProcDef sizeT], [UQPL.ProcDef holeT sizeT])

loweredProcs :: Lens' (LoweringOutput holeT sizeT) [ProcDef sizeT]
loweredProcs = _1

loweredUProcs :: Lens' (LoweringOutput holeT sizeT) [UQPL.ProcDef holeT sizeT]
loweredUProcs = _2

{- | Monad to compile ProtoQB to CQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT holeT sizeT costT = MyReaderWriterStateT (LoweringEnv holeT sizeT costT) (LoweringOutput holeT sizeT) (LoweringCtx sizeT) (Either String)

-- | Generate a new identifier with the given prefix.
newIdent :: forall holeT sizeT costT. Ident -> CompilerT holeT sizeT costT Ident
newIdent prefix = do
  ident <-
    msum . map checked $
      prefix : map ((prefix <>) . ("_" <>) . show) [1 :: Int ..]
  uniqNames . at ident ?= ()
  return ident
 where
  checked :: Ident -> CompilerT holeT sizeT costT Ident
  checked name = do
    already_exists <- use (uniqNames . at name)
    case already_exists of
      Nothing -> return name
      Just () -> throwError "next ident please!"

-- | Add a new procedure.
addProc :: ProcDef sizeT -> CompilerT holeT sizeT costT ()
addProc procDef = tellAt loweredProcs [procDef]

-- ================================================================================
-- Compilation
-- ================================================================================

-- | Lower a source function to a procedure call.
lowerFunDef ::
  (P.TypeCheckable sizeT, Show costT, Floating costT) =>
  -- | fail prob
  costT ->
  -- | source function
  P.FunDef sizeT ->
  CompilerT holeT sizeT costT Ident
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
  (P.TypeCheckable sizeT, Show costT, Floating costT) =>
  -- | fail prob
  costT ->
  -- | source function name
  Ident ->
  CompilerT holeT sizeT costT Ident
lowerFunDefByName eps f = do
  fun_def <- view $ protoFunCtx . Ctx.at f . singular _Just
  lowerFunDef eps fun_def

-- | Lower a source expression to a statement.
lowerExpr ::
  (P.TypeCheckable sizeT, Show costT, Floating costT) =>
  -- fail prob
  costT ->
  -- source expression
  P.Expr sizeT ->
  -- return variables
  [Ident] ->
  CompilerT holeT sizeT costT (Stmt sizeT)
lowerExpr _ P.VarE{P.arg} [ret] = return $ AssignS [ret] $ VarE arg
lowerExpr _ P.ConstE{P.val} [ret] = return $ AssignS [ret] $ ConstE val
lowerExpr _ P.UnOpE{P.un_op, P.arg} [ret] =
  let expr = case un_op of
        P.NotOp -> NotE (VarE arg)
   in return $ AssignS{rets = [ret], expr}
lowerExpr eps P.FunCallE{P.fun_kind = P.FunctionCall f, P.args} rets = do
  proc_name <- lowerFunDefByName eps f
  return $ CallS{fun = FunctionCall proc_name, args = args ++ rets, meta_params = []}
lowerExpr eps P.FunCallE{P.fun_kind = P.PrimitiveCall "any" [predicate], P.args} rets = do
  -- the predicate
  pred_fun@P.FunDef{P.param_types} <-
    view (protoFunCtx . Ctx.at predicate)
      >>= maybeWithError ("cannot find predicate " <> predicate)

  -- size of the search space
  let s_ty@(P.Fin n) = last param_types

  -- fail prob of search
  let eps_s = eps / 2

  -- fail prob predicate
  let eps_pred = eps - eps_s
  max_cost_formula <- view $ qsearchConfig . to costFormulas . to P.qSearchWorstCaseCost
  let n_max_pred_calls = max_cost_formula n eps_pred
  let eps_per_pred_call = eps_pred / n_max_pred_calls
  let delta_per_pred_call = eps_per_pred_call / 2 -- norm error in unitary predicate

  -- lower the unitary predicate
  let upred_compiler = UQPL.lowerFunDef delta_per_pred_call pred_fun
  (pred_uproc, uprocs) <- do
    uenv <- view id <&> _3 %~ unitaryImpl
    ust <- use id
    (a, _, w) <- lift $ runMyReaderWriterStateT upred_compiler uenv ust
    return (a, w)

  tellAt loweredUProcs uprocs
  let pred_proc_name = pred_uproc ^. to UQPL.lowered_def . to UQPL.proc_name

  -- emit the QSearch algorithm
  qsearch_builder <- view $ qsearchConfig . to qsearchAlgo
  qsearch_params <- forM (args ++ rets) $ \x -> do
    ty <- use $ typingCtx . Ctx.at x . singular _Just
    return (x, ty)
  let qsearch_proc =
        qsearch_builder
          s_ty
          0
          eps_s
          (\x b -> error "TODO unitary pred call")
          (\x b -> HoleS "classical predicate call")
          qsearch_params
  qsearch_proc_name <- newIdent $ printf "QSearch[%s]" (show eps_s)
  addProc $ qsearch_proc{proc_name = qsearch_proc_name}

  return
    CallS
      { fun = FunctionCall qsearch_proc_name
      , args = args ++ rets
      }
lowerExpr _ e _ = error $ "TODO implement lowerExpr " <> show e

-- | Lower a single statement
lowerStmt ::
  (P.TypeCheckable sizeT, Show costT, Floating costT) =>
  costT ->
  P.Stmt sizeT ->
  CompilerT holeT sizeT costT (Stmt sizeT)
-- single statement
lowerStmt eps s@P.ExprS{P.rets, P.expr} = do
  checker <- P.checkStmt <$> view protoFunCtx <*> pure s
  zoom typingCtx $ embedStateT checker
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
  forall holeT sizeT costT.
  (P.TypeCheckable sizeT, Show costT, Floating costT) =>
  -- | the implementation of primitive `any`
  QSearchCQImpl holeT sizeT costT ->
  -- | input bindings to the source program
  P.TypingCtx sizeT ->
  -- | name of the oracle
  P.OracleName ->
  -- | fail prob \( \varepsilon \)
  costT ->
  -- | source program
  P.Program sizeT ->
  Either String (Program holeT sizeT, P.TypingCtx sizeT)
lowerProgram qsearch_config gamma_in oracle_name eps prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config = (funCtx, oracle_name, qsearch_config)
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
