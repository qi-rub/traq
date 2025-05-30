{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.UnitaryQPL.Lowering (
  -- * Types
  CompilerT,

  -- ** Compiler State
  LoweringConfig,
  LoweringCtx,
  LoweringOutput,

  -- ** Helpers
  newIdent,
  addProc,
  allocAncillaWithPref,
  allocAncilla,
  ControlFlag (..),

  -- ** Lenses
  protoFunCtx,
  typingCtx,

  -- * Compilation
  Lowerable (..),
  LoweredProc (..),
  lowerExpr,
  lowerStmt,
  lowerFunDef,
  lowerProgram,

  -- * extra
  withTag,
) where

import Control.Monad (forM, msum, unless, when)
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
import QCompose.UnitaryQPL.Syntax

-- | Configuration for lowering
type LoweringConfig primT holeT sizeT costT = (P.FunCtx primT sizeT, P.OracleName)

protoFunCtx :: Lens' (LoweringConfig primT holeT sizeT costT) (P.FunCtx primT sizeT)
protoFunCtx = _1

oracleName :: Lens' (LoweringConfig primT holeT sizeT costT) P.OracleName
oracleName = _2

{- | A global lowering context storing the source functions and generated procedures
 along with info to generate unique ancilla and variable/procedure names
-}
type LoweringCtx sizeT = (Set.Set Ident, P.TypingCtx sizeT)

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
class
  (P.UnitaryCostablePrimitive primsT primT sizeT costT) =>
  Lowerable primsT primT holeT sizeT costT
  where
  lowerPrimitive ::
    costT ->
    primT ->
    -- | args
    [Ident] ->
    -- | rets
    [Ident] ->
    CompilerT primsT holeT sizeT costT (Stmt holeT sizeT)

instance (Show costT) => Lowerable primsT Void holeT sizeT costT where
  lowerPrimitive _ = absurd

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
allocAncillaWithPref :: Ident -> P.VarType sizeT -> CompilerT primT holeT sizeT costT Ident
allocAncillaWithPref pref ty = do
  name <- newIdent pref
  zoom typingCtx $ Ctx.put name ty
  return name

-- | Allocate an ancilla register @aux_<<n>>@, and update the typing context.
allocAncilla :: P.VarType sizeT -> CompilerT primT holeT sizeT costT Ident
allocAncilla = allocAncillaWithPref "aux"

-- | Add a new procedure.
addProc :: ProcDef holeT sizeT -> CompilerT primT holeT sizeT costT ()
addProc procDef = tellAt loweredProcs [procDef]

-- ================================================================================
-- Lowering
-- ================================================================================

-- | A procDef generated from a funDef, along with the partitioned register spaces.
data LoweredProc holeT sizeT costT = LoweredProc
  { lowered_def :: ProcDef holeT sizeT
  , has_ctrl :: Bool
  , inp_tys :: [P.VarType sizeT]
  -- ^ the inputs to the original fun
  , out_tys :: [P.VarType sizeT]
  -- ^ the outputs of the original fun
  , aux_tys :: [P.VarType sizeT]
  -- ^ all other registers
  }

data ControlFlag = WithControl | WithoutControl deriving (Eq, Show, Read, Enum)

-- | Compile a single expression statement
lowerExpr ::
  forall primsT holeT sizeT costT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  costT ->
  P.Expr primsT sizeT ->
  -- | returns
  [Ident] ->
  CompilerT primsT holeT sizeT costT (Stmt holeT sizeT)
-- basic expressions
lowerExpr _ P.VarE{P.arg} [ret] = do
  ty <- zoom typingCtx $ Ctx.lookup arg
  return $ UnitaryS [arg, ret] $ RevEmbedU $ IdF ty
lowerExpr _ P.ConstE{P.val, P.ty} [ret] =
  return $ UnitaryS [ret] $ RevEmbedU $ ConstF ty (MetaValue val)
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
  LoweredProc{lowered_def, inp_tys, out_tys, aux_tys} <- lowerFunDef WithoutControl delta fun

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
-- primitive call
lowerExpr delta P.FunCallE{P.fun_kind = P.PrimitiveCall prim, P.args} rets =
  lowerPrimitive delta prim args rets
-- error out in all other cases
lowerExpr _ _ _ = throwError "cannot compile unsupported expression"

-- | Compile a statement (simple or compound)
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
  forall primsT sizeT costT holeT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  -- | precision \delta
  costT ->
  P.FunDef primsT sizeT ->
  CompilerT primsT holeT sizeT costT (LoweredProc holeT sizeT costT)
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

      let procDef = ProcDef{proc_name, proc_meta_params = [], proc_params = all_binds, mproc_body = Just proc_body, is_oracle = False}
      addProc procDef

      return
        LoweredProc
          { lowered_def = procDef
          , has_ctrl = False
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
  forall primsT sizeT costT holeT.
  ( Lowerable primsT primsT holeT sizeT costT
  , P.TypeCheckable sizeT
  , Show costT
  , Floating costT
  ) =>
  -- | Controlled?
  ControlFlag ->
  -- | precision \delta
  costT ->
  P.FunDef primsT sizeT ->
  CompilerT primsT holeT sizeT costT (LoweredProc holeT sizeT costT)
-- lower a declaration as-is, we treat all declarations as perfect data oracles (so delta is ignored).
lowerFunDef with_ctrl _ P.FunDef{P.fun_name, P.param_types, P.ret_types, P.mbody = Nothing} = do
  let param_names = map (printf "in_%d") [0 .. length param_types]
  let ret_names = map (printf "out_%d") [0 .. length ret_types]
  is_oracle <- (fun_name ==) <$> view oracleName

  ctrl_qubit <- newIdent "ctrl"
  let proc_def =
        ProcDef
          { proc_name = (case with_ctrl of WithControl -> "Ctrl_"; _ -> "") ++ fun_name
          , proc_meta_params = []
          , proc_params =
              [(ctrl_qubit, ParamCtrl, P.tbool) | with_ctrl == WithControl]
                ++ withTag ParamInp (zip param_names param_types)
                ++ withTag ParamOut (zip ret_names ret_types)
          , mproc_body = Nothing
          , is_oracle
          }

  addProc proc_def
  return
    LoweredProc
      { lowered_def = proc_def
      , has_ctrl = with_ctrl == WithControl
      , inp_tys = param_types
      , out_tys = ret_types
      , aux_tys = []
      }
lowerFunDef
  with_ctrl
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
    proc_name <- newIdent $ printf "%s%s_clean[%s]" (case with_ctrl of WithControl -> "Ctrl_"; _ -> "") fun_name (show delta)

    g_ret_names <- mapM allocAncilla g_ret_tys

    g_aux_names <- mapM allocAncilla g_aux_tys

    let g_args = param_names ++ g_ret_names ++ g_aux_names

    ctrl_qubit <- newIdent "ctrl"
    let copy_op ty = (case with_ctrl of WithControl -> Controlled; _ -> id) (RevEmbedU $ IdF ty)

    -- call g, copy and uncompute g
    let proc_body =
          SeqS
            [ CallS{proc_id = g_dirty_name, dagger = False, args = g_args}
            , SeqS -- copy all the return values
                [ UnitaryS ([ctrl_qubit | with_ctrl == WithControl] ++ [x, x']) (copy_op ty)
                | (x, x', ty) <- zip3 g_ret_names ret_names g_ret_tys
                ]
            , CallS{proc_id = g_dirty_name, dagger = True, args = g_args}
            ]

    is_oracle <- (fun_name ==) <$> view oracleName
    let proc_def =
          ProcDef
            { proc_name
            , proc_meta_params = []
            , proc_params =
                [(ctrl_qubit, ParamCtrl, P.tbool) | with_ctrl == WithControl]
                  ++ withTag ParamInp param_binds
                  ++ withTag ParamOut (zip ret_names g_ret_tys)
                  ++ withTag ParamAux (zip g_ret_names g_ret_tys ++ zip g_aux_names g_aux_tys)
            , mproc_body = Just proc_body
            , is_oracle
            }
    addProc proc_def
    return
      LoweredProc
        { lowered_def = proc_def
        , has_ctrl = with_ctrl == WithControl
        , inp_tys = map snd param_binds
        , out_tys = g_ret_tys
        , aux_tys = g_ret_tys ++ g_aux_tys
        }

-- | Lower a full program into a UQPL program.
lowerProgram ::
  forall primsT holeT costT.
  ( Lowerable primsT primsT holeT SizeT costT
  , Show costT
  , Floating costT
  ) =>
  -- | All variable bindings
  P.TypingCtx SizeT ->
  -- | the name of the oracle
  P.OracleName ->
  -- | precision \delta
  costT ->
  P.Program primsT SizeT ->
  Either String (Program holeT SizeT, P.TypingCtx SizeT)
lowerProgram gamma_in oracle_name delta prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        undefined
          & protoFunCtx .~ funCtx
          & oracleName .~ oracle_name
  let ctx =
        undefined
          & typingCtx .~ gamma_in
          & uniqNames .~ P.allNamesP prog
  let compiler = lowerStmt delta stmt
  (stmtU, ctx', outputU) <- runMyReaderWriterStateT compiler config ctx
  return
    ( Program
        { proc_defs = outputU ^. loweredProcs . to (Ctx.fromListWith proc_name)
        , stmt = stmtU
        }
    , ctx' ^. typingCtx
    )
