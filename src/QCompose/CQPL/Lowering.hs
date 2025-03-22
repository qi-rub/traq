module QCompose.CQPL.Lowering where

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl
import Text.Printf (printf)

import QCompose.CQPL.Syntax
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Context
import QCompose.Utils.MonadHelpers

convType :: P.VarType a -> VarType
convType (P.Fin _) = IntT

-- | Formulas for primitives
newtype QSearchCQImpl = QSearchCQImpl
  { costFormulas :: P.QSearchFormulas SizeT Float
  }

_costFormulas :: Lens' QSearchCQImpl (P.QSearchFormulas SizeT Float)
_costFormulas = lens costFormulas (\s a -> s{costFormulas = a})

-- | Configuration for lowering
type LoweringConfig a = (P.FunCtx a, QSearchCQImpl)

protoFunCtx :: Lens' (LoweringConfig a) (P.FunCtx a)
protoFunCtx = _1

qsearchConfig :: Lens' (LoweringConfig a) QSearchCQImpl
qsearchConfig = _2

{- | A global lowering context storing the source functions and generated procedures
 along with info to generate unique ancilla and variable/procedure names
-}
type LoweringCtx a = (Set.Set Ident, P.TypingCtx a)

emptyLoweringCtx :: LoweringCtx a
emptyLoweringCtx = (Set.empty, Map.empty)

uniqNames :: Lens' (LoweringCtx a) (Set.Set Ident)
uniqNames = _1

typingCtx :: Lens' (LoweringCtx a) (P.TypingCtx a)
typingCtx = _2

-- | The outputs of lowering
type LoweringOutput a = ([ProcDef a], [UQPL.ProcDef a])

loweredProcs :: Lens' (LoweringOutput a) [ProcDef a]
loweredProcs = _1

{- | Monad to compile ProtoQB to UQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT a = RWST (LoweringConfig a) (LoweringOutput a) (LoweringCtx a) (Either String)

newIdent :: Ident -> CompilerT a Ident
newIdent prefix = do
  ident <-
    msum . map checked $
      prefix : map ((prefix <>) . ("_" <>) . show) [1 :: Int ..]
  uniqNames . at ident ?= ()
  return ident
 where
  checked :: Ident -> CompilerT a Ident
  checked name = do
    already_exists <- use (uniqNames . at name)
    case already_exists of
      Nothing -> return name
      Just () -> throwError "next ident please!"

-- | Allocate an ancilla register, and update the typing context.
allocAncilla :: P.VarType a -> CompilerT a Ident
allocAncilla ty = do
  name <- newIdent "aux"
  zoom typingCtx $ putValue name ty
  return name

-- | Add a new procedure.
addProc :: ProcDef a -> CompilerT a ()
addProc procDef = tell $ mempty & loweredProcs .~ [procDef]

-- | lower an oracle declaration to a uqpl oracle decl
lowerOracleDecl :: P.OracleDecl a -> OracleDecl
lowerOracleDecl P.OracleDecl{P.param_types, P.ret_types} =
  OracleDecl{oracle_param_types = map convType $ param_types ++ ret_types}

-- | A procDef generated from a funDef, along with the partitioned register spaces.
data LoweredProc a = LoweredProc
  { lowered_def :: ProcDef a
  , inp_tys :: [P.VarType a]
  -- ^ the inputs to the original fun
  , out_tys :: [P.VarType a]
  -- ^ the outputs of the original fun
  , aux_tys :: [P.VarType a]
  -- ^ all other registers
  }

-- | Compile a single expression statement
lowerExpr ::
  Precision ->
  P.Expr SizeT ->
  -- | returns
  [Ident] ->
  CompilerT SizeT Stmt
-- basic expressions
lowerExpr _ P.VarE{P.arg} [ret] =
  return $ AssignS [ret] (VarE arg)
lowerExpr _ P.ConstE{P.val} [ret] =
  return $ AssignS [ret] (ConstE val)
lowerExpr _ P.UnOpE{P.un_op, P.arg} [ret] =
  let un_op_expr = case un_op of P.NotOp -> NotE (VarE arg)
   in return $ AssignS [ret] un_op_expr
lowerExpr _ P.BinOpE{P.bin_op, P.lhs, P.rhs} [ret] =
  let bin_op_expr = case bin_op of
        P.AddOp -> AddE (VarE lhs) (VarE rhs)
        P.LEqOp -> LEqE (VarE lhs) (VarE rhs)
        P.AndOp -> AndE (VarE lhs) (VarE rhs)
   in return $ AssignS [ret] bin_op_expr
-- oracle call
lowerExpr _ P.FunCallE{P.fun_kind = P.OracleCall, P.args} rets =
  return $ CallS OracleCall (args ++ rets)
-- function call
-- lowerExpr delta P.FunCallE{P.fun_kind = P.FunctionCall f, P.args} rets = do
--   fun <- view protoFunCtx >>= (lift . P.lookupFun f)
--   LoweredProc{lowered_def, inp_tys, out_tys, aux_tys} <- lowerFunDef delta fun
--   addProc lowered_def

--   when (length inp_tys /= length args) $
--     throwError "mismatched number of args"
--   when (length out_tys /= length rets) $
--     throwError "mismatched number of rets"

--   aux_args <- forM aux_tys allocAncilla
--   return
--     CallS
--       { proc_id = proc_name lowered_def
--       , args = args ++ rets ++ aux_args
--       , dagger = False
--       }

-- `any`
-- lowerExpr delta P.FunCallE{P.fun_kind = P.PrimitiveCall P.Contains, P.args = (predicate : args)} rets = do
--   -- the predicate
--   pred_fun@P.FunDef{P.param_binds} <- view protoFunCtx >>= (lift . P.lookupFun predicate)

--   -- size of the search space
--   let s_ty@(P.Fin n) = param_binds & last & snd

--   -- precision for the search
--   let delta_search = delta / 2
--   -- precision for each predicate call
--   calc_u_cost <- view qsearchConfig <&> costFormulas <&> P.qSearchUnitaryCost
--   let n_qry = calc_u_cost n delta_search
--   let delta_per_pred_call = (delta - delta_search) / (2 * n_qry)

--   -- compile the predicate
--   LoweredProc
--     { lowered_def = pred_proc
--     , inp_tys = pred_inp_tys
--     , out_tys = pred_out_tys
--     , aux_tys = pred_aux_tys
--     } <-
--     lowerFunDef delta_per_pred_call pred_fun

--   when (pred_out_tys /= [P.tbool]) $ throwError "invalid outputs for predicate"
--   when (last pred_inp_tys /= s_ty) $ throwError "mismatched search argument type"

--   addProc pred_proc

--   -- emit the qsearch procedure
--   -- TODO maybe this can be somehow "parametrized" so we don't have to generate each time.
--   qsearch_proc_name <-
--     newIdent $
--       printf "QSearch[%d, %e, %s]" n delta_search (proc_name pred_proc)
--   qsearch_ancilla_tys <- view (qsearchConfig . _ancillaTypes) <&> (\f -> f n delta_search)
--   let qsearch_param_tys =
--         init pred_inp_tys
--           ++ pred_out_tys
--           ++ [s_ty]
--           ++ pred_aux_tys
--           ++ qsearch_ancilla_tys

--   qsearch_param_names <- replicateM (length qsearch_param_tys) $ newIdent "_qs"
--   addProc
--     ProcDef
--       { proc_name = qsearch_proc_name
--       , proc_params = zip qsearch_param_names qsearch_param_tys
--       , proc_body =
--           UnitaryS qsearch_param_names $
--             BlackBoxU $
--               QSearchBB (proc_name pred_proc) n_qry
--       }

--   qsearch_ancilla <- mapM allocAncilla qsearch_ancilla_tys
--   pred_ancilla <- mapM allocAncilla pred_aux_tys
--   search_elem <- allocAncilla s_ty
--   return
--     CallS
--       { proc_id = qsearch_proc_name
--       , args = args ++ rets ++ [search_elem] ++ pred_ancilla ++ qsearch_ancilla
--       , dagger = False
--       }

-- error out in all other cases
lowerExpr _ e rets = throwError $ "cannot compile unsupported expression: " <> show (e, rets)

-- | Compile a statement (simple or compound)
lowerStmt ::
  Precision ->
  P.Stmt SizeT ->
  CompilerT SizeT Stmt
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
  proc_name <- newIdent $ printf "%s[%e]" fun_name delta
  (proc_body, all_binds, aux_tys) <- withSandboxOf typingCtx $ do
    typingCtx .= Map.fromList param_binds

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
    aux_binds <- use typingCtx <&> Map.toList <&> filter (not . (`elem` param_names ++ ret_names) . fst)
    let all_binds = param_binds ++ ret_binds ++ aux_binds

    return (proc_body', all_binds, map snd aux_binds)

  return
    LoweredProc
      { lowered_def = ProcDef{proc_name, proc_params = all_binds, proc_body}
      , inp_tys = map snd param_binds
      , out_tys = map snd ret_binds
      , aux_tys = aux_tys
      }

-- | Lower a full program into a UQPL program.
lowerProgram ::
  QSearchCQImpl ->
  P.TypingCtx SizeT ->
  Precision ->
  P.Program SizeT ->
  Either String (Program SizeT, P.TypingCtx SizeT)
lowerProgram qsearch_config gamma_in delta prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"
  (stmtU, ctx', outputU) <- runRWST compiler config ctx
  return
    ( Program
        { oracle_decl = funCtx & P.oracle_decl & lowerOracleDecl
        , proc_defs = outputU ^. loweredProcs
        , stmt = stmtU
        }
    , ctx' ^. typingCtx
    )
 where
  config = (funCtx, qsearch_config)
  ctx =
    emptyLoweringCtx
      & typingCtx .~ gamma_in
      & uniqNames .~ P.allNamesP prog
  compiler = lowerStmt delta stmt
