module QCompose.UnitaryQPL.Lowering where

import Control.Monad.Except (throwError)
import Control.Monad.RWS
import Data.List (intersect)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl
import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import QCompose.Control.MonadHelpers
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.ProtoLang.Syntax (FunctionCallKind (PrimitiveCall))
import QCompose.UnitaryQPL.Syntax

-- | Formulas for primitives
data QSearchUnitaryImpl = QSearchUnitaryImpl
  { ancillaTypes :: SizeT -> Precision -> [P.VarType SizeT]
  , costFormulas :: P.QSearchFormulas SizeT Complexity
  }

_ancillaTypes :: Lens' QSearchUnitaryImpl (SizeT -> Precision -> [P.VarType SizeT])
_ancillaTypes = lens ancillaTypes (\s a -> s{ancillaTypes = a})

_costFormulas :: Lens' QSearchUnitaryImpl (P.QSearchFormulas SizeT Complexity)
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
type LoweringCtx a = (Set.Set Ident, P.TypingCtx a)

emptyLoweringCtx :: LoweringCtx a
emptyLoweringCtx = (Set.empty, Ctx.empty)

uniqNames :: Lens' (LoweringCtx a) (Set.Set Ident)
uniqNames = _1

typingCtx :: Lens' (LoweringCtx a) (P.TypingCtx a)
typingCtx = _2

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
  zoom typingCtx $ Ctx.put name ty
  return name

-- | Add a new procedure.
addProc :: ProcDef a -> CompilerT a ()
addProc procDef = tell [procDef]

-- | lower an oracle declaration to a uqpl oracle decl
lowerOracleDecl :: P.OracleDecl a -> OracleDecl a
lowerOracleDecl P.OracleDecl{P.param_types, P.ret_types} =
  OracleDecl{param_types = param_types ++ ret_types}

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
  CompilerT SizeT (Stmt SizeT)
-- basic expressions
lowerExpr _ P.VarE{P.arg} [ret] = do
  ty <- zoom typingCtx $ Ctx.lookup arg
  return $ UnitaryS [arg, ret] $ RevEmbedU $ IdF ty
lowerExpr _ P.ConstE{P.val, P.ty} [ret] =
  return $ UnitaryS [ret] $ RevEmbedU $ ConstF ty val
lowerExpr _ P.UnOpE{P.un_op, P.arg} [ret] = do
  ty <- zoom typingCtx $ Ctx.lookup arg
  return $ UnitaryS [arg, ret] $ RevEmbedU $ case un_op of P.NotOp -> NotF ty
lowerExpr _ P.BinOpE{P.bin_op, P.lhs, P.rhs} [ret] = do
  ty <- zoom typingCtx $ Ctx.lookup lhs
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
  fun <-
    view (protoFunCtx . to P.fun_defs . Ctx.at f)
      >>= maybeWithError ("cannot find function " <> f)
  LoweredProc{lowered_def, inp_tys, out_tys, aux_tys} <- lowerFunDef delta fun

  when (length inp_tys /= length args) $
    throwError "mismatched number of args"
  when (length out_tys /= length rets) $
    throwError "mismatched number of rets"

  aux_args <- forM aux_tys allocAncilla
  return
    CallS
      { proc_id = proc_name lowered_def
      , args = args ++ rets ++ aux_args
      , dagger = False
      }

-- `any`
lowerExpr delta P.FunCallE{P.fun_kind = PrimitiveCall P.Contains, P.args = (predicate : args)} rets = do
  -- the predicate
  pred_fun@P.FunDef{P.param_binds} <-
    view (protoFunCtx . to P.fun_defs . Ctx.at predicate)
      >>= maybeWithError ("cannot find predicate " <> predicate)

  -- size of the search space
  let s_ty@(P.Fin n) = param_binds & last & snd

  -- precision for the search
  let delta_search = delta / 2
  -- precision for each predicate call
  calc_u_cost <- view qsearchConfig <&> costFormulas <&> P.qSearchUnitaryCost
  let n_qry = calc_u_cost n delta_search
  let delta_per_pred_call = (delta - delta_search) / (2 * n_qry)

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

  -- emit the qsearch procedure
  -- TODO maybe this can be somehow "parametrized" so we don't have to generate each time.
  qsearch_proc_name <-
    newIdent $
      printf "QSearch[%d, %e, %s]" n delta_search (proc_name pred_proc)
  qsearch_ancilla_tys <- view (qsearchConfig . _ancillaTypes) <&> (\f -> f n delta_search)
  let qsearch_param_tys =
        init pred_inp_tys
          ++ pred_out_tys
          ++ [s_ty]
          ++ pred_aux_tys
          ++ qsearch_ancilla_tys

  qsearch_param_names <- replicateM (length qsearch_param_tys) $ newIdent "_qs"
  addProc
    ProcDef
      { proc_name = qsearch_proc_name
      , proc_params = withTag ParamUnk $ zip qsearch_param_names qsearch_param_tys
      , proc_body =
          UnitaryS qsearch_param_names $
            BlackBoxU $
              QSearchBB (proc_name pred_proc) n_qry
      }

  qsearch_ancilla <- mapM allocAncilla qsearch_ancilla_tys
  pred_ancilla <- mapM allocAncilla pred_aux_tys
  search_elem <- allocAncilla s_ty
  return
    CallS
      { proc_id = qsearch_proc_name
      , args = args ++ rets ++ [search_elem] ++ pred_ancilla ++ qsearch_ancilla
      , dagger = False
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

 This can produce entangled aux registers.

 TODO try to cache compiled procs by key (funDefName, Precision).
-}
lowerFunDefWithGarbage ::
  Precision ->
  P.FunDef SizeT ->
  CompilerT SizeT (LoweredProc SizeT)
lowerFunDefWithGarbage delta P.FunDef{P.fun_name, P.param_binds, P.ret_binds, P.body} = withSandboxOf typingCtx $ do
  proc_name <- newIdent $ printf "%s[%e]" fun_name delta

  typingCtx .= Ctx.fromList param_binds
  proc_body <- lowerStmt delta body
  let param_names = map fst param_binds
  let ret_names = map fst ret_binds
  when (param_names `intersect` ret_names /= []) $
    throwError "function should not return parameters!"

  aux_binds <- use typingCtx <&> Ctx.toList <&> filter (not . (`elem` param_names ++ ret_names) . fst)
  let all_binds = withTag ParamInp param_binds ++ withTag ParamOut ret_binds ++ withTag ParamAux aux_binds

  let procDef = ProcDef{proc_name, proc_params = all_binds, proc_body}
  addProc procDef

  return
    LoweredProc
      { lowered_def = procDef
      , inp_tys = map snd param_binds
      , out_tys = map snd ret_binds
      , aux_tys = map snd aux_binds
      }

withTag :: ParamTag -> [(Ident, P.VarType a)] -> [(Ident, ParamTag, P.VarType a)]
withTag tag = map $ \(x, ty) -> (x, tag, ty)

{- | Compile a single function definition with the given precision.
 Each invocation will generate a new proc, even if an identical one exists.

 The auxillary registers are uncomputed.

 TODO try to cache compiled procs by key (funDefName, Precision).
-}
lowerFunDef ::
  Precision ->
  P.FunDef SizeT ->
  CompilerT SizeT (LoweredProc SizeT)
lowerFunDef delta fun@P.FunDef{P.fun_name, P.param_binds, P.ret_binds} = withSandboxOf typingCtx $ do
  -- get the proc call that computes with garbage
  LoweredProc{lowered_def, aux_tys = g_aux_tys, out_tys = g_ret_tys} <- lowerFunDefWithGarbage (delta / 2) fun
  let g_dirty_name = lowered_def ^. to proc_name

  typingCtx .= Ctx.fromList (param_binds ++ ret_binds)

  proc_name <- newIdent $ printf "%s_clean[%e]" fun_name delta

  let g_params_names = map fst param_binds
  let ret_names = map fst ret_binds

  g_ret_names <- mapM allocAncilla g_ret_tys

  g_aux_names <- mapM allocAncilla g_aux_tys

  let g_args = g_params_names ++ g_ret_names ++ g_aux_names

  -- call g, copy and uncompute g
  let proc_body =
        SeqS
          [ CallS{proc_id = g_dirty_name, dagger = False, args = g_args}
          , SeqS -- copy all the return values
              [ UnitaryS [x, x'] (RevEmbedU $ IdF ty)
              | (x, x', ty) <- zip3 g_ret_names ret_names g_ret_tys
              ]
          , CallS{proc_id = g_dirty_name, dagger = True, args = g_args}
          ]

  let proc_def =
        ProcDef
          { proc_name
          , proc_params =
              withTag ParamInp param_binds
                ++ zip3 ret_names (repeat ParamOut) g_ret_tys
                ++ zip3 g_ret_names (repeat ParamAux) g_ret_tys
                ++ zip3 g_aux_names (repeat ParamAux) g_aux_tys
          , proc_body
          }
  addProc proc_def
  return
    LoweredProc
      { lowered_def = proc_def
      , inp_tys = map snd param_binds
      , out_tys = g_ret_tys
      , aux_tys = g_ret_tys ++ g_aux_tys
      }

-- | Lower a full program into a UQPL program.
lowerProgram ::
  QSearchUnitaryImpl ->
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
