{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Compiler.Unitary (
  -- * Types
  CompilerT,

  -- ** Compiler State
  LoweringConfig,
  LoweringCtx,
  LoweringOutput,

  -- ** Helpers
  newIdent,
  allocAncillaWithPref,
  allocAncilla,
  ControlFlag (..),

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

import Control.Monad (forM, unless, when, zipWithM)
import Control.Monad.Except (throwError)
import Data.Foldable (Foldable (toList))
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import qualified Traq.CQPL as CQPL
import Traq.CQPL.Syntax
import Traq.Compiler.Utils
import Traq.Prelude
import qualified Traq.ProtoLang as P
import Traq.UnitaryQPL.Syntax

-- | Configuration for lowering
type LoweringConfig primT sizeT costT = P.UnitaryCostEnv primT sizeT costT

{- | Monad to compile ProtoQB to UQPL programs.
This should contain the _final_ typing context for the input program,
that is, contains both the inputs and outputs of each statement.
-}
type CompilerT primT holeT sizeT costT = MyReaderWriterStateT (LoweringConfig primT sizeT costT) (LoweringOutput holeT sizeT costT) (LoweringCtx sizeT) (Either String)

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
    CompilerT primsT holeT sizeT costT (UStmt holeT sizeT)

instance (Show costT) => Lowerable primsT Void holeT sizeT costT where
  lowerPrimitive _ = absurd

-- ================================================================================
-- Helpers
-- ================================================================================

-- | Allocate an ancilla register, and update the typing context.
allocAncillaWithPref :: Ident -> P.VarType sizeT -> CompilerT primT holeT sizeT costT Ident
allocAncillaWithPref pref ty = do
  name <- newIdent pref
  zoom P._typingCtx $ Ctx.put name ty
  return name

-- | Allocate an ancilla register @aux_<<n>>@, and update the typing context.
allocAncilla :: P.VarType sizeT -> CompilerT primT holeT sizeT costT Ident
allocAncilla = allocAncillaWithPref "aux"

-- ================================================================================
-- Lowering
-- ================================================================================

-- | A procDef generated from a funDef, along with the partitioned register spaces.
data LoweredProc holeT sizeT costT = LoweredProc
  { lowered_def :: ProcDef holeT sizeT costT
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
  CompilerT primsT holeT sizeT costT (UStmt holeT sizeT)
-- basic expressions are lowered to their unitary embedding
lowerExpr _ P.BasicExprE{P.basic_expr} rets = do
  let args = toList $ P.freeVarsBE basic_expr
  return $ UnitaryS{args = args ++ rets, unitary = RevEmbedU args basic_expr}

-- function call
lowerExpr delta P.FunCallE{P.fun_kind = P.FunctionCall fun_name, P.args} rets = do
  fun <-
    view (P._funCtx . Ctx.at fun_name)
      >>= maybeWithError ("cannot find function " <> fun_name)
  LoweredProc{lowered_def, inp_tys, out_tys, aux_tys} <- lowerFunDef WithoutControl delta fun_name fun

  when (length inp_tys /= length args) $
    throwError "mismatched number of args"
  when (length out_tys /= length rets) $
    throwError "mismatched number of rets"

  aux_args <- forM aux_tys allocAncilla
  return
    UCallS
      { proc_id = proc_name lowered_def
      , args = args ++ rets ++ aux_args
      , dagger = False
      }
-- primitive call
lowerExpr delta P.FunCallE{P.fun_kind = P.PrimitiveCall prim, P.args} rets =
  lowerPrimitive delta prim args rets

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
  CompilerT primsT holeT sizeT costT (UStmt holeT sizeT)
-- single statement
lowerStmt delta s@P.ExprS{P.rets, P.expr} = do
  censored . magnify P._funCtx . zoom P._typingCtx $ P.typeCheckStmt s
  lowerExpr delta expr rets

-- compound statements
lowerStmt delta (P.SeqS ss) = do
  deltas <- P.splitEps delta ss
  USeqS <$> zipWithM lowerStmt deltas ss

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
  -- | source function name
  Ident ->
  -- | function
  P.FunDef primsT sizeT ->
  CompilerT primsT holeT sizeT costT (LoweredProc holeT sizeT costT)
lowerFunDefWithGarbage _ fun_name P.FunDef{P.param_types, P.ret_types, P.mbody = Nothing} = do
  tick <- view $ P._unitaryTicks . at fun_name . to (fromMaybe 0)

  proc_name <- newIdent fun_name

  let proc_def =
        ProcDef
          { info_comment = ""
          , proc_name
          , proc_meta_params = []
          , proc_param_types = param_types ++ ret_types
          , proc_body = ProcBodyU $ UProcDecl tick
          }

  addProc proc_def
  return
    LoweredProc
      { lowered_def = proc_def
      , has_ctrl = False
      , inp_tys = param_types
      , out_tys = ret_types
      , aux_tys = []
      }
lowerFunDefWithGarbage
  delta
  fun_name
  P.FunDef
    { P.param_types
    , P.ret_types
    , P.mbody =
      Just P.FunBody{P.param_names, P.ret_names, P.body_stmt}
    } =
    withSandboxOf P._typingCtx $ do
      proc_name <- newIdent fun_name
      let info_comment = printf "%s[%s]" fun_name (show delta)

      let param_binds = zip param_names param_types
      let ret_binds = zip ret_names ret_types

      P._typingCtx .= Ctx.fromList param_binds
      proc_body <- lowerStmt delta body_stmt
      when (param_names `intersect` ret_names /= []) $
        throwError "function should not return parameters!"

      aux_binds <- use (P._typingCtx . to Ctx.toList) <&> filter (not . (`elem` param_names ++ ret_names) . fst)
      let all_binds = withTag ParamInp param_binds ++ withTag ParamOut ret_binds ++ withTag ParamAux aux_binds

      let procDef =
            ProcDef
              { info_comment
              , proc_name
              , proc_meta_params = []
              , proc_param_types = map (view _3) all_binds
              , proc_body =
                  ProcBodyU $
                    UProcBody
                      { uproc_param_names = map (view _1) all_binds
                      , uproc_param_tags = map (view _2) all_binds
                      , uproc_body_stmt = proc_body
                      }
              }
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
  -- | function name
  Ident ->
  -- | function
  P.FunDef primsT sizeT ->
  CompilerT primsT holeT sizeT costT (LoweredProc holeT sizeT costT)
lowerFunDef
  with_ctrl
  delta
  fun_name
  fun@P.FunDef
    { P.param_types
    , P.ret_types
    , P.mbody
    } = withSandboxOf P._typingCtx $ do
    -- get the proc call that computes with garbage
    LoweredProc{lowered_def, aux_tys = g_aux_tys, out_tys = g_ret_tys} <- lowerFunDefWithGarbage (delta / 2) fun_name fun
    let g_dirty_name = lowered_def ^. to proc_name

    let param_names = case mbody of
          Just P.FunBody{P.param_names} -> param_names
          Nothing -> map (printf "in_%d") [0 .. length param_types - 1]
    let ret_names = case mbody of
          Just P.FunBody{P.ret_names} -> ret_names
          Nothing -> map (printf "out_%d") [0 .. length ret_types - 1]

    let param_binds = zip param_names param_types
    let ret_binds = zip ret_names ret_types

    P._typingCtx .= Ctx.fromList (param_binds ++ ret_binds)
    proc_name <- newIdent fun_name
    let info_comment = printf "%sClean[%s, %s]" (case with_ctrl of WithControl -> "Ctrl_"; _ -> "") fun_name (show delta)

    g_ret_names <- mapM allocAncilla g_ret_tys

    g_aux_names <- mapM allocAncilla g_aux_tys

    let g_args = param_names ++ g_ret_names ++ g_aux_names

    ctrl_qubit <- newIdent "ctrl"
    let copy_op = (case with_ctrl of WithControl -> Controlled; _ -> id) (RevEmbedU ["a"] (P.VarE "a"))

    -- call g, copy and uncompute g
    let proc_body =
          USeqS
            [ UCallS{proc_id = g_dirty_name, dagger = False, args = g_args}
            , USeqS -- copy all the return values
                [ UnitaryS ([ctrl_qubit | with_ctrl == WithControl] ++ [x, x']) copy_op
                | (x, x') <- zip g_ret_names ret_names
                ]
            , UCallS{proc_id = g_dirty_name, dagger = True, args = g_args}
            ]

    let all_params =
          [(ctrl_qubit, ParamCtrl, P.tbool) | with_ctrl == WithControl]
            ++ withTag ParamInp param_binds
            ++ withTag ParamOut (zip ret_names g_ret_tys)
            ++ withTag ParamAux (zip g_ret_names g_ret_tys ++ zip g_aux_names g_aux_tys)
    let proc_def =
          ProcDef
            { info_comment
            , proc_name
            , proc_meta_params = []
            , proc_param_types = map (view _3) all_params
            , proc_body =
                ProcBodyU $
                  UProcBody
                    { uproc_param_names = map (view _1) all_params
                    , uproc_param_tags = map (view _2) all_params
                    , uproc_body_stmt = proc_body
                    }
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
  P.PrecisionSplittingStrategy ->
  -- | All variable bindings
  P.TypingCtx SizeT ->
  -- | the costs of each declaration
  P.OracleTicks costT ->
  -- | precision \delta
  costT ->
  P.Program primsT SizeT ->
  Either String (CQPL.Program holeT SizeT costT)
lowerProgram strat gamma_in oracle_ticks delta prog@P.Program{P.funCtx, P.stmt} = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        default_
          & (P._funCtx .~ funCtx)
          & (P._unitaryTicks .~ oracle_ticks)
          & (P._precSplitStrat .~ strat)
  let ctx =
        default_
          & (P._typingCtx .~ gamma_in)
          & (_uniqNamesCtx .~ P.allNamesP prog)

  let compiler = lowerStmt delta stmt
  (stmtU, ctx', outputU) <- runMyReaderWriterStateT compiler config ctx

  let procs = outputU ^. _loweredProcs . to (Ctx.fromListWith proc_name)
  let (arg_names, arg_tys) = ctx' ^. P._typingCtx . to Ctx.toList . to unzip
  let main_proc =
        ProcDef
          { info_comment = ""
          , proc_name = "main"
          , proc_meta_params = []
          , proc_param_types = arg_tys
          , proc_body =
              ProcBodyU $
                UProcBody
                  { uproc_param_names = arg_names
                  , uproc_param_tags = replicate (length arg_names) ParamUnk
                  , uproc_body_stmt = stmtU
                  }
          }

  return CQPL.Program{CQPL.proc_defs = procs & Ctx.ins "main" .~ main_proc}
