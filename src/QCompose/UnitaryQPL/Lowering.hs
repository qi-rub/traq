module QCompose.UnitaryQPL.Lowering where

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import Control.Monad.Trans (lift)
import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.Mtl

import Data.Maybe (catMaybes)
import QCompose.Basic
import qualified QCompose.ProtoLang as P
import QCompose.ProtoLang.Syntax (FunctionCallKind (SubroutineCall))
import QCompose.UnitaryQPL.Syntax
import QCompose.Utils.Context
import QCompose.Utils.MonadHelpers

-- | Formulas for primitives
data QSearchUnitaryImpl = QSearchUnitaryImpl
  { ancillaTypes :: SizeT -> Precision -> [P.VarType SizeT]
  , costFormulas :: P.QSearchFormulas
  }

_ancillaTypes :: Lens' QSearchUnitaryImpl (SizeT -> Precision -> [P.VarType SizeT])
_ancillaTypes = lens ancillaTypes (\s a -> s{ancillaTypes = a})

_costFormulas :: Lens' QSearchUnitaryImpl P.QSearchFormulas
_costFormulas = lens costFormulas (\s a -> s{costFormulas = a})

-- | Configuration for lowering
type LoweringConfig a = (P.FunCtx a, QSearchUnitaryImpl)

protoFunCtx :: Lens' (LoweringConfig a) (P.FunCtx a)
protoFunCtx = _1

qsearchConfig :: Lens' (LoweringConfig a) QSearchUnitaryImpl
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

newIdent :: Ident -> CompilerT a Ident
newIdent prefix = do
  i <- use uniqNameIdx
  uniqNameIdx += 1
  return $ prefix ++ "_" ++ show i

-- | Allocate an ancilla register, and update the typing context.
allocAncilla :: P.VarType a -> CompilerT a Ident
allocAncilla ty = do
  i <- use ancillaIdx
  ancillaIdx += 1

  let name = "_aux_" <> show i
  zoom typingCtx $ putValue name ty
  return name

-- | Add a new procedure.
addProc :: ProcDef a -> CompilerT a ()
addProc procDef = tell [procDef]

-- | A procDef generated from a funDef, along with the partitioned register spaces.
data LoweredProc a = LoweredProc
  { lowered_def :: ProcDef a
  , -- | the inputs to the original fun
    inp_tys :: [P.VarType a]
  , -- | the outputs of the original fun
    out_tys :: [P.VarType a]
  , -- | all other registers
    aux_tys :: [P.VarType a]
  }

-- | Compile a single expression statement
lowerExpr ::
  Precision ->
  P.Expr SizeT ->
  -- | returns
  [Ident] ->
  CompilerT SizeT (Stmt SizeT)
-- basic expressions
lowerExpr _ P.VarE{P.arg} [ret] = do
  ty <- zoom typingCtx $ lookupVar arg
  return $ UnitaryS [arg, ret] $ RevEmbedU $ IdF ty
lowerExpr _ P.ConstE{P.val, P.ty} [ret] =
  return $ UnitaryS [ret] $ RevEmbedU $ ConstF ty val
lowerExpr _ P.UnOpE{P.un_op, P.arg} [ret] = do
  ty <- zoom typingCtx $ lookupVar arg
  return $ UnitaryS [arg, ret] $ RevEmbedU $ case un_op of P.NotOp -> NotF ty
lowerExpr _ P.BinOpE{P.bin_op, P.lhs, P.rhs} [ret] = do
  ty <- zoom typingCtx $ lookupVar lhs
  return $
    UnitaryS [lhs, rhs, ret] $ case bin_op of
      P.AddOp -> RevEmbedU $ AddF ty
      P.LEqOp -> RevEmbedU $ LEqF ty
      P.AndOp -> Toffoli

-- oracle call
lowerExpr _ P.FunCallE{P.fun_kind = P.OracleCall, P.args} rets = do
  return $ UnitaryS (args ++ rets) Oracle

-- function call
lowerExpr delta P.FunCallE{P.fun_kind = P.FunctionCall f, P.args} rets = do
  fun <- view protoFunCtx >>= (lift . P.lookupFun f)
  LoweredProc{lowered_def, inp_tys, out_tys, aux_tys} <- lowerFunDef delta fun
  addProc lowered_def

  guard $ length inp_tys == length args
  guard $ length out_tys == length rets

  aux_args <- forM aux_tys allocAncilla
  return
    CallS
      { proc_id = proc_name lowered_def
      , args = args ++ rets ++ aux_args
      }

-- `any`
lowerExpr delta P.FunCallE{P.fun_kind = SubroutineCall P.Contains, P.args = (predicate : args)} rets = do
  -- the predicate
  pred_fun@P.FunDef{P.param_binds} <- view protoFunCtx >>= (lift . P.lookupFun predicate)

  -- size of the search space
  let s_ty@(P.Fin n) = param_binds & last & snd

  -- precision for the search
  let delta_search = delta / 2
  -- precision for each predicate call
  delta_per_pred_call <- do
    calc_u_cost <- view qsearchConfig <&> costFormulas <&> P.qSearchUnitaryCost
    let n_qry = calc_u_cost n delta_search
    return $ (delta - delta_search) / (2 * n_qry)

  -- compile the predicate
  LoweredProc
    { lowered_def = pred_proc
    , inp_tys = pred_inp_tys
    , out_tys = pred_out_tys
    , aux_tys = pred_aux_tys
    } <-
    lowerFunDef delta_per_pred_call pred_fun

  when (pred_out_tys /= [P.tbool]) $ throwError "invalid outputs for predicate"
  when (last pred_inp_tys /= s_ty) $ throwError "mismatched search argument type"

  addProc pred_proc

  -- emit the qsearch procedure
  -- TODO maybe this can be somehow "parametrized" so we don't have to generate each time.
  qsearch_proc_name <- newIdent "_qsearch_"
  qsearch_ancilla_tys <- view (qsearchConfig . _ancillaTypes) <&> (\f -> f n delta_search)
  let qsearch_param_tys =
        init pred_inp_tys
          ++ pred_out_tys
          ++ [s_ty]
          ++ pred_aux_tys
          ++ qsearch_ancilla_tys

  qsearch_param_names <- replicateM (length qsearch_param_tys) $ newIdent "_qsearch_arg_"
  addProc
    ProcDef
      { proc_name = qsearch_proc_name
      , proc_params = zip qsearch_param_names qsearch_param_tys
      , proc_body = UnitaryS qsearch_param_names $ BlackBox $ "QSearch[" <> proc_name pred_proc <> "]"
      }

  qsearch_ancilla <- mapM allocAncilla qsearch_ancilla_tys
  pred_ancilla <- mapM allocAncilla pred_aux_tys
  search_elem <- allocAncilla s_ty
  return
    CallS
      { proc_id = qsearch_proc_name
      , args = args ++ rets ++ [search_elem] ++ pred_ancilla ++ qsearch_ancilla
      }

-- error out in all other cases
lowerExpr _ e rets = throwError $ "cannot compile unsupported expression: " <> show (e, rets)

-- | Compile a statement (simple or compound)
lowerStmt ::
  Precision ->
  P.Stmt SizeT ->
  CompilerT SizeT (Stmt SizeT)
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

{- | Compile a single function definition with the given precision.
 Each invocation will generate a new proc, even if an identical one exists.

 TODO try to cache compiled procs by key (funDefName, Precision).
-}
lowerFunDef ::
  Precision ->
  P.FunDef SizeT ->
  CompilerT SizeT (LoweredProc SizeT)
lowerFunDef delta P.FunDef{P.fun_name, P.param_binds, P.ret_binds, P.body} = do
  proc_name <- newIdent fun_name
  (proc_body, all_binds, aux_tys) <- withSandboxOf typingCtx $ do
    typingCtx .= M.fromList param_binds

    proc_body <- lowerStmt delta body

    let param_names = map fst param_binds
    rets <- forM ret_binds $ \(x, ty) -> do
      -- if a parameter is directly returned, then make a copy
      if x `elem` param_names
        then do
          x' <- allocAncilla ty
          let copyS = UnitaryS [x, x'] $ RevEmbedU $ IdF ty
          return (x', Just copyS)
        else return (x, Nothing)
    let (ret_names, copiesS) = unzip rets & over _2 catMaybes

    let proc_body' = SeqS [proc_body, SeqS copiesS]
    all_binds <- use typingCtx <&> M.toList
    aux_tys <- use typingCtx <&> M.toList <&> filter (not . (`elem` param_names ++ ret_names) . fst) <&> map snd

    return (proc_body', all_binds, aux_tys)

  return
    LoweredProc
      { lowered_def = ProcDef{proc_name, proc_params = all_binds, proc_body}
      , inp_tys = map snd param_binds
      , out_tys = map snd ret_binds
      , aux_tys = aux_tys
      }

-- | Lower a full program into a UQPL program.
lowerProgram ::
  QSearchUnitaryImpl ->
  P.TypingCtx SizeT ->
  Precision ->
  P.Program SizeT ->
  Either String (Program SizeT)
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
