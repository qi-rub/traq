{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Compiler.Unitary (
  -- * Types
  CompilerT,

  -- ** Compiler State
  LoweringEnv,
  LoweringCtx,
  LoweringOutput,

  -- ** Helpers
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
import Control.Monad.RWS (RWST (..))
import Data.Foldable (Foldable (toList))
import Data.List (intersect)
import GHC.Generics hiding (to)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import qualified Traq.Analysis as A
import qualified Traq.CQPL as CQPL
import Traq.CQPL.Syntax
import Traq.Compiler.Utils
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Primitives that support a unitary lowering.
class
  ( P.TypeInferrable ext sizeT
  , sizeT ~ SizeType ext
  , precT ~ PrecType ext
  ) =>
  Lowerable ext sizeT precT
    | ext -> sizeT precT
  where
  lowerPrimitive ::
    forall ext' m.
    ( Lowerable ext' sizeT precT
    , m ~ CompilerT ext'
    , SizeType ext' ~ sizeT
    , PrecType ext' ~ precT
    ) =>
    A.L2NormError precT ->
    ext ->
    -- | rets
    [Ident] ->
    m (UStmt sizeT)
  default lowerPrimitive ::
    forall ext' m.
    ( Lowerable ext' sizeT precT
    , m ~ CompilerT ext'
    , Generic ext
    , GLowerable (Rep ext) sizeT precT
    , SizeType ext' ~ sizeT
    , PrecType ext' ~ precT
    ) =>
    A.L2NormError precT ->
    ext ->
    -- | rets
    [Ident] ->
    m (UStmt sizeT)
  lowerPrimitive delta p = glowerPrimitive (from p) delta

instance (P.TypingReqs sizeT) => Lowerable (P.Core sizeT precT) sizeT precT where
  lowerPrimitive _ = \case {}

-- | Generic
class GLowerable f sizeT precT | f -> sizeT precT where
  glowerPrimitive ::
    forall primT ext' m.
    ( Lowerable ext' sizeT precT
    , m ~ CompilerT ext'
    , SizeType ext' ~ sizeT
    , PrecType ext' ~ precT
    ) =>
    f primT ->
    A.L2NormError precT ->
    -- | rets
    [Ident] ->
    m (UStmt sizeT)

instance
  (GLowerable f1 sizeT precT, GLowerable f2 sizeT precT) =>
  GLowerable (f1 :+: f2) sizeT precT
  where
  glowerPrimitive (L1 p) = glowerPrimitive p
  glowerPrimitive (R1 p) = glowerPrimitive p

instance
  (GLowerable f sizeT precT) =>
  GLowerable (M1 i c f) sizeT precT
  where
  glowerPrimitive (M1 x) = glowerPrimitive x

instance
  (Lowerable f sizeT precT) =>
  GLowerable (K1 i f) sizeT precT
  where
  glowerPrimitive (K1 x) delta = lowerPrimitive delta x

-- ================================================================================
-- Helpers
-- ================================================================================

-- | Allocate an ancilla register, and update the typing context.
allocAncillaWithPref :: (sizeT ~ SizeType ext) => Ident -> P.VarType sizeT -> CompilerT ext Ident
allocAncillaWithPref pref ty = do
  name <- newIdent pref
  zoom P._typingCtx $ Ctx.put name ty
  return name

-- | Allocate an ancilla register @aux_<<n>>@, and update the typing context.
allocAncilla :: (sizeT ~ SizeType ext) => P.VarType sizeT -> CompilerT ext Ident
allocAncilla = allocAncillaWithPref "aux"

-- ================================================================================
-- Lowering
-- ================================================================================

-- | A procDef generated from a funDef, along with the partitioned register spaces.
data LoweredProc sizeT = LoweredProc
  { lowered_def :: ProcDef sizeT
  , has_ctrl :: Bool
  , inp_tys :: [P.VarType sizeT]
  -- ^ the inputs to the original fun
  , out_tys :: [P.VarType sizeT]
  -- ^ the outputs of the original fun
  , aux_tys :: [P.VarType sizeT]
  -- ^ all other registers
  }

type instance SizeType (LoweredProc sizeT) = sizeT

data ControlFlag = WithControl | WithoutControl deriving (Eq, Show, Read, Enum)

-- | Compile a single expression statement
lowerExpr ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  ) =>
  A.L2NormError precT ->
  P.Expr ext ->
  -- | returns
  [Ident] ->
  CompilerT ext (UStmt sizeT)
-- basic expressions are lowered to their unitary embedding
lowerExpr _ P.BasicExprE{P.basic_expr} rets = do
  let args = toList $ P.freeVars basic_expr
  return $ UnitaryS{qargs = args ++ rets, unitary = RevEmbedU args basic_expr}

-- random sampling expressions
lowerExpr _ P.RandomSampleE{P.distr_expr = P.UniformE{}} rets = do
  return $ UnitaryS{qargs = rets, unitary = Unif}
lowerExpr _ P.RandomSampleE{P.distr_expr = P.BernoulliE{}} rets = do
  return $ UnitaryS{qargs = rets, unitary = error "TODO: Add Ry gate"}

-- function call
lowerExpr delta P.FunCallE{fname, P.args} rets = do
  fun <- P.lookupFunE fname
  LoweredProc{lowered_def, inp_tys, out_tys, aux_tys} <- lowerFunDef WithoutControl delta fname fun

  when (length inp_tys /= length args) $
    throwError "mismatched number of args"
  when (length out_tys /= length rets) $
    throwError "mismatched number of rets"

  aux_args <- forM aux_tys allocAncilla
  return
    UCallS
      { uproc_id = proc_name lowered_def
      , qargs = args ++ rets ++ aux_args
      , dagger = False
      }
-- primitive call
lowerExpr delta P.PrimCallE{prim} rets =
  lowerPrimitive delta prim rets
-- unsupported
lowerExpr _ _ _ = error "TODO: unsupported"

-- | Compile a statement (simple or compound)
lowerStmt ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  ) =>
  A.L2NormError precT ->
  P.Stmt ext ->
  CompilerT ext (UStmt sizeT)
-- single statement
lowerStmt delta s@P.ExprS{P.rets, P.expr} = do
  _ <- magnify P._funCtx . zoom P._typingCtx . ignoreWriter $ P.inferTypes s
  lowerExpr delta expr rets

-- compound statements
lowerStmt delta (P.SeqS ss) = do
  deltas <- A.splitEps delta ss
  USeqS <$> zipWithM lowerStmt deltas ss

-- unsupported
lowerStmt _ _ = error "lowering: unsupported"

{- | Compile a single function definition with the given precision.
 Each invocation will generate a new proc, even if an identical one exists.

 This can produce entangled aux registers.

 TODO try to cache compiled procs by key (funDefName, Precision).
-}
lowerFunDefWithGarbage ::
  forall ext sizeT precT m.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  , m ~ CompilerT ext
  ) =>
  -- | precision \delta
  A.L2NormError precT ->
  -- | source function name
  Ident ->
  -- | function
  P.FunDef ext ->
  m (LoweredProc sizeT)
lowerFunDefWithGarbage _ fun_name P.FunDef{P.param_types, P.ret_types, P.mbody = Nothing} = do
  proc_name <- newIdent fun_name

  let proc_def =
        ProcDef
          { info_comment = ""
          , proc_name
          , proc_meta_params = []
          , proc_param_types = param_types ++ ret_types
          , proc_body = ProcBodyU UProcDecl
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
      let info_comment = printf "%s[%s]" fun_name (show $ A.getL2NormError delta)

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
  forall ext sizeT precT m.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  , m ~ CompilerT ext
  ) =>
  -- | Controlled?
  ControlFlag ->
  -- | precision \delta
  A.L2NormError precT ->
  -- | function name
  Ident ->
  -- | function
  P.FunDef ext ->
  m (LoweredProc sizeT)
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
    LoweredProc{lowered_def, aux_tys = g_aux_tys, out_tys = g_ret_tys} <- lowerFunDefWithGarbage (delta `A.divideError` 2) fun_name fun
    let g_dirty_name = lowered_def ^. to proc_name

    let param_names = case mbody of
          Just P.FunBody{P.param_names = ps} -> ps
          Nothing -> map (printf "in_%d") [0 .. length param_types - 1]
    let ret_names = case mbody of
          Just P.FunBody{P.ret_names = rs} -> rs
          Nothing -> map (printf "out_%d") [0 .. length ret_types - 1]

    let param_binds = zip param_names param_types
    let ret_binds = zip ret_names ret_types

    P._typingCtx .= Ctx.fromList (param_binds ++ ret_binds)
    proc_name <- newIdent fun_name
    let info_comment = printf "%sClean[%s, %s]" (case with_ctrl of WithControl -> "Ctrl_"; _ -> "") fun_name (show $ A.getL2NormError delta)

    g_ret_names <- mapM allocAncilla g_ret_tys

    g_aux_names <- mapM allocAncilla g_aux_tys

    let g_args = param_names ++ g_ret_names ++ g_aux_names

    ctrl_qubit <- newIdent "ctrl"
    let copy_op = (case with_ctrl of WithControl -> Controlled; _ -> id) (RevEmbedU ["a"] (P.VarE "a"))

    -- call g, copy and uncompute g
    let proc_body =
          USeqS
            [ UCallS{uproc_id = g_dirty_name, dagger = False, qargs = g_args}
            , USeqS -- copy all the return values
                [ UnitaryS ([ctrl_qubit | with_ctrl == WithControl] ++ [x, x']) copy_op
                | (x, x') <- zip g_ret_names ret_names
                ]
            , UCallS{uproc_id = g_dirty_name, dagger = True, qargs = g_args}
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

-- | Lower a full program into a unitary CQPL program.
lowerProgram ::
  forall ext precT.
  ( Lowerable ext SizeT precT
  , Show precT
  , Floating precT
  , P.HasFreeVars ext
  ) =>
  A.PrecisionSplittingStrategy ->
  -- | All variable bindings
  P.TypingCtx SizeT ->
  -- | precision \delta
  A.L2NormError precT ->
  P.Program ext ->
  Either String (CQPL.Program SizeT)
lowerProgram strat gamma_in delta prog@(P.Program fs) = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        default_
          & (P._funCtx .~ P.namedFunsToFunCtx fs)
          & (A._precSplitStrat .~ strat)
  let ctx =
        default_
          & (P._typingCtx .~ gamma_in)
          & (_uniqNamesCtx .~ P.allNamesP prog)

  let P.NamedFunDef{P.fun_name = main_name, P.fun_def = main_def} = last fs
  (_, _, outputU) <-
    lowerFunDefWithGarbage delta main_name main_def
      & (\m -> runRWST m config ctx)
  let procs = outputU ^. _loweredProcs . to (Ctx.fromListWith proc_name)
  return CQPL.Program{CQPL.proc_defs = procs}
