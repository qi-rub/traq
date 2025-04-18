{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.UnitaryQPL.Lowering where

import Control.Monad (forM, msum, replicateM, unless, when)
import Control.Monad.Except (throwError)
import Data.List (intersect)
import qualified Data.Set as Set
import Data.Void (Void, absurd)
import Lens.Micro
import Lens.Micro.Mtl
import Text.Printf (printf)

import qualified QCompose.Data.Context as Ctx

import QCompose.Control.Monad
import QCompose.Prelude
import qualified QCompose.ProtoLang as P
import QCompose.ProtoLang.Syntax (FunctionCallKind (PrimitiveCallOld))
import QCompose.UnitaryQPL.Syntax

-- | Formulas for primitives
data QSearchUnitaryImpl holeT sizeT costT = QSearchUnitaryImpl
  { ancillaTypes ::
      sizeT -> -- size of search space
      costT -> -- precision
      [P.VarType sizeT]
  , costFormulas :: P.QSearchFormulas sizeT costT
  , algorithm :: P.VarType sizeT -> (Ident -> Ident -> Stmt holeT sizeT) -> costT -> Stmt holeT sizeT
  }

-- | Configuration for lowering
type LoweringConfig primT holeT sizeT costT = (P.FunCtx primT sizeT, P.OracleName, QSearchUnitaryImpl holeT sizeT costT)

protoFunCtx :: Lens' (LoweringConfig primT holeT sizeT costT) (P.FunCtx primT sizeT)
protoFunCtx = _1

oracleName :: Lens' (LoweringConfig primT holeT sizeT costT) P.OracleName
oracleName = _2

qsearchConfig :: Lens' (LoweringConfig primT holeT sizeT costT) (QSearchUnitaryImpl holeT sizeT costT)
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
type LoweringOutput holeT sizeT = [ProcDef holeT sizeT]

loweredProcs :: Lens' (LoweringOutput holeT sizeT) [ProcDef holeT sizeT]
loweredProcs = id

{- | Monad to compile ProtoQB to UQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT primT holeT sizeT costT = MyReaderWriterStateT (LoweringConfig primT holeT sizeT costT) (LoweringOutput holeT sizeT) (LoweringCtx sizeT) (Either String)

-- | Primitives that support a unitary lowering.
class (P.UnitaryCostablePrimitive primT sizeT costT) => Lowerable primT sizeT costT where
  lowerPrimitive ::
    forall primsT holeT.
    primT ->
    CompilerT primsT holeT sizeT costT (Stmt holeT sizeT)

instance Lowerable Void sizeT costT where
  lowerPrimitive = absurd

-- ================================================================================
-- Helpers
-- ================================================================================

newIdent :: Ident -> CompilerT primT holeT sizeT costT Ident
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

-- | Allocate an ancilla register, and update the typing context.
allocAncilla :: P.VarType sizeT -> CompilerT primT holeT sizeT costT Ident
allocAncilla ty = do
  name <- newIdent "aux"
  zoom typingCtx $ Ctx.put name ty
  return name

-- | Add a new procedure.
addProc :: ProcDef holeT sizeT -> CompilerT primT holeT sizeT costT ()
addProc procDef = tellAt loweredProcs [procDef]

-- ================================================================================
-- Lowering
-- ================================================================================

-- | A procDef generated from a funDef, along with the partitioned register spaces.
data LoweredProc holeT sizeT costT = LoweredProc
  { lowered_def :: ProcDef holeT sizeT
  , inp_tys :: [P.VarType sizeT]
  -- ^ the inputs to the original fun
  , out_tys :: [P.VarType sizeT]
  -- ^ the outputs of the original fun
  , aux_tys :: [P.VarType sizeT]
  -- ^ all other registers
  }

-- | Compile a single expression statement
lowerExpr ::
  forall primT holeT sizeT costT.
  ( Lowerable primT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  costT ->
  P.Expr primT sizeT ->
  -- | returns
  [Ident] ->
  CompilerT primT holeT sizeT costT (Stmt holeT sizeT)
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
lowerExpr _ P.TernaryE{P.branch, P.lhs, P.rhs} [ret] = do
  ty <- zoom typingCtx $ Ctx.lookup lhs
  let c_copy = Controlled $ RevEmbedU $ IdF ty
  return . SeqS $
    [ UnitaryS [branch] XGate
    , UnitaryS [branch, lhs, ret] c_copy
    , UnitaryS [branch] XGate
    , UnitaryS [branch, rhs, ret] c_copy
    ]

-- function call
lowerExpr delta P.FunCallE{P.fun_kind = P.FunctionCall f, P.args} rets = do
  fun <-
    view (protoFunCtx . Ctx.at f)
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
lowerExpr delta P.FunCallE{P.fun_kind = PrimitiveCallOld "any" [predicate], P.args} rets = do
  -- the predicate
  pred_fun@P.FunDef{P.param_types} <-
    view (protoFunCtx . Ctx.at predicate)
      >>= maybeWithError ("cannot find predicate " <> predicate)

  -- size of the search space
  let s_ty@(P.Fin n) = last param_types

  -- precision for the search
  let delta_search = delta / 2
  -- precision for each predicate call
  calc_u_cost <- view $ qsearchConfig . to costFormulas . to P.qSearchUnitaryCost
  let n_qry = (calc_u_cost :: sizeT -> costT -> costT) n delta_search
  let delta_per_pred_call = (delta - delta_search) / n_qry

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
      printf "QSearch[%s, %s, %s]" (show n) (show delta_search) (proc_name pred_proc)
  qsearch_ancilla_tys <- view (qsearchConfig . to ancillaTypes) <&> (\f -> f n delta_search)
  let qsearch_param_tys =
        init pred_inp_tys
          ++ pred_out_tys
          ++ pred_aux_tys
          ++ qsearch_ancilla_tys

  qsearch_ancilla <- mapM allocAncilla qsearch_ancilla_tys
  pred_ancilla <- mapM allocAncilla pred_aux_tys

  qsearch_param_names <- replicateM (length qsearch_param_tys) $ newIdent "_qs"
  mk_proc_body <- view $ qsearchConfig . to algorithm
  let proc_body =
        mk_proc_body
          s_ty
          (\x b -> CallS{proc_id = proc_name pred_proc, dagger = False, args = args ++ [x, b] ++ pred_ancilla})
          delta_search
  addProc
    ProcDef
      { proc_name = qsearch_proc_name
      , proc_params = withTag ParamUnk $ zip qsearch_param_names qsearch_param_tys
      , mproc_body = Just proc_body
      , is_oracle = False
      }

  return
    CallS
      { proc_id = qsearch_proc_name
      , args = args ++ rets ++ pred_ancilla ++ qsearch_ancilla
      , dagger = False
      }

-- error out in all other cases
lowerExpr _ _ _ = throwError "cannot compile unsupported expression"

-- | Compile a statement (simple or compound)
lowerStmt ::
  ( Lowerable primT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  costT ->
  P.Stmt primT sizeT ->
  CompilerT primT holeT sizeT costT (Stmt holeT sizeT)
-- single statement
lowerStmt delta s@P.ExprS{P.rets, P.expr} = do
  censored . magnify protoFunCtx . zoom typingCtx $ P.checkStmt s
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
  ( Lowerable primT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  -- | precision \delta
  costT ->
  P.FunDef primT sizeT ->
  CompilerT primT holeT sizeT costT (LoweredProc holeT sizeT costT)
lowerFunDefWithGarbage _ P.FunDef{P.mbody = Nothing} = error "TODO"
lowerFunDefWithGarbage
  delta
  P.FunDef
    { P.fun_name
    , P.param_types
    , P.ret_types
    , P.mbody =
      Just P.FunBody{P.param_names, P.ret_names, P.body_stmt}
    } =
    withSandboxOf typingCtx $ do
      proc_name <- newIdent $ printf "%s[%s]" fun_name (show delta)

      let param_binds = zip param_names param_types
      let ret_binds = zip ret_names ret_types

      typingCtx .= Ctx.fromList param_binds
      proc_body <- lowerStmt delta body_stmt
      when (param_names `intersect` ret_names /= []) $
        throwError "function should not return parameters!"

      aux_binds <- use typingCtx <&> Ctx.toList <&> filter (not . (`elem` param_names ++ ret_names) . fst)
      let all_binds = withTag ParamInp param_binds ++ withTag ParamOut ret_binds ++ withTag ParamAux aux_binds

      let procDef = ProcDef{proc_name, proc_params = all_binds, mproc_body = Just proc_body, is_oracle = False}
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
  ( Lowerable primT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  -- | precision \delta
  costT ->
  P.FunDef primT sizeT ->
  CompilerT primT holeT sizeT costT (LoweredProc holeT sizeT costT)
-- lower a declaration as-is, we treat all declarations as perfect data oracles (so delta is ignored).
lowerFunDef _ P.FunDef{P.fun_name, P.param_types, P.ret_types, P.mbody = Nothing} = do
  let param_names = map (printf "in_%d") [0 .. length param_types]
  let ret_names = map (printf "out_%d") [0 .. length ret_types]
  is_oracle <- (fun_name ==) <$> view oracleName
  let proc_def =
        ProcDef
          { proc_name = fun_name
          , proc_params =
              withTag ParamInp (zip param_names param_types)
                ++ withTag ParamOut (zip ret_names ret_types)
          , mproc_body = Nothing
          , is_oracle
          }

  addProc proc_def
  return
    LoweredProc
      { lowered_def = proc_def
      , inp_tys = param_types
      , out_tys = ret_types
      , aux_tys = []
      }
lowerFunDef
  delta
  fun@P.FunDef
    { P.fun_name
    , P.param_types
    , P.ret_types
    , P.mbody = Just P.FunBody{P.param_names, P.ret_names}
    } = withSandboxOf typingCtx $ do
    -- get the proc call that computes with garbage
    LoweredProc{lowered_def, aux_tys = g_aux_tys, out_tys = g_ret_tys} <- lowerFunDefWithGarbage (delta / 2) fun
    let g_dirty_name = lowered_def ^. to proc_name

    let param_binds = zip param_names param_types
    let ret_binds = zip ret_names ret_types

    typingCtx .= Ctx.fromList (param_binds ++ ret_binds)

    proc_name <- newIdent $ printf "%s_clean[%s]" fun_name (show delta)

    g_ret_names <- mapM allocAncilla g_ret_tys

    g_aux_names <- mapM allocAncilla g_aux_tys

    let g_args = param_names ++ g_ret_names ++ g_aux_names

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

    is_oracle <- (fun_name ==) <$> view oracleName
    let proc_def =
          ProcDef
            { proc_name
            , proc_params =
                withTag ParamInp param_binds
                  ++ withTag ParamOut (zip ret_names g_ret_tys)
                  ++ withTag ParamAux (zip g_ret_names g_ret_tys ++ zip g_aux_names g_aux_tys)
            , mproc_body = Just proc_body
            , is_oracle
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
  ( Lowerable primT SizeT costT
  , Show costT
  , Floating costT
  ) =>
  -- | A unitary implementation for QSearch (`any`)
  QSearchUnitaryImpl holeT SizeT costT ->
  -- | All variable bindings
  P.TypingCtx SizeT ->
  -- | the name of the oracle
  P.OracleName ->
  -- | precision \delta
  costT ->
  P.Program primT SizeT ->
  Either String (Program holeT SizeT, P.TypingCtx SizeT)
lowerProgram qsearch_config gamma_in oracle_name delta prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"
  (stmtU, ctx', outputU) <- runMyReaderWriterStateT compiler config ctx
  return
    ( Program
        { proc_defs = outputU ^. loweredProcs . to (Ctx.fromListWith proc_name)
        , stmt = stmtU
        }
    , ctx' ^. typingCtx
    )
 where
  config = (funCtx, oracle_name, qsearch_config)
  ctx =
    emptyLoweringCtx
      & typingCtx .~ gamma_in
      & uniqNames .~ P.allNamesP prog
  compiler = lowerStmt delta stmt
