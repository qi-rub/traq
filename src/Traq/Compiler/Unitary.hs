{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Compiler.Unitary (
  -- * Types
  CompilerT,
  CompileU (..),
  compileU1,

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
  lowerProgramU,

  -- * extra
  withTag,
) where

import Control.Monad (forM, unless, when, zipWithM)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (RWST (..))
import Data.Foldable (Foldable (toList))
import Data.List (intersect)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import qualified Traq.CQPL as CQPL
import Traq.CQPL.Syntax
import Traq.Compiler.Prelude
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
    ext ->
    -- | rets
    [Ident] ->
    m (UStmt sizeT)

instance (P.TypingReqs sizeT) => Lowerable (P.Core sizeT precT) sizeT precT where
  lowerPrimitive = \case {}

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

-- | Allocate fresh set of auxiliaries corresponding to the types of given vars.
freshAux :: (m ~ CompilerT ext) => [Ident] -> m [Ident]
freshAux xs = do
  tys <- zoom P._typingCtx $ mapM Ctx.lookup xs
  zipWithM allocAncillaWithPref xs tys

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
  P.Expr ext ->
  -- | returns
  [Ident] ->
  CompilerT ext (UStmt sizeT)
-- basic expressions are lowered to their unitary embedding
lowerExpr P.BasicExprE{P.basic_expr} rets = do
  let args = toList $ P.freeVars basic_expr
  return $ UnitaryS{qargs = args ++ rets, unitary = RevEmbedU args basic_expr}

-- random sampling expressions
lowerExpr P.RandomSampleE{P.distr_expr = distr} rets = do
  return $ UnitaryS{qargs = rets, unitary = DistrU distr}

-- function call
lowerExpr P.FunCallE{fname, P.args} rets = do
  fun <- P.lookupFunE fname
  LoweredProc{lowered_def, inp_tys, out_tys, aux_tys} <- lowerFunDefWithGarbage fname fun

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
lowerExpr P.PrimCallE{prim} rets =
  lowerPrimitive prim rets
-- unsupported
lowerExpr _ _ = error "TODO: unsupported"

-- | Compile a statement (simple or compound)
lowerStmt ::
  forall ext sizeT precT.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  ) =>
  P.Stmt ext ->
  CompilerT ext (UStmt sizeT)
-- single statement
lowerStmt s@P.ExprS{P.rets, P.expr} = do
  _ <- magnify P._funCtx . zoom P._typingCtx . ignoreWriter $ P.inferTypes s
  lowerExpr expr rets

-- compound statements
lowerStmt (P.SeqS ss) = USeqS <$> mapM lowerStmt ss
-- unsupported
lowerStmt _ = error "lowering: unsupported"

{- | Compile a single function definition with the given precision.
 Each invocation will generate a new proc, even if an identical one exists.

 This can produce entangled aux registers.
-}
lowerFunDefWithGarbage ::
  forall ext sizeT precT m.
  ( Lowerable ext sizeT precT
  , P.TypingReqs sizeT
  , Show precT
  , Floating precT
  , m ~ CompilerT ext
  ) =>
  -- | source function name
  Ident ->
  -- | function
  P.FunDef ext ->
  m (LoweredProc sizeT)
lowerFunDefWithGarbage fun_name P.FunDef{P.param_types, P.ret_types, P.mbody = Nothing} = do
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
  fun_name
  P.FunDef
    { P.param_types
    , P.ret_types
    , P.mbody =
      Just P.FunBody{P.param_names, P.ret_names, P.body_stmt}
    } =
    withSandboxOf P._typingCtx $ do
      proc_name <- newIdent fun_name
      let info_comment = printf "%s" fun_name

      let param_binds = zip param_names param_types
      let ret_binds = zip ret_names ret_types

      P._typingCtx .= Ctx.fromList param_binds
      proc_body <- lowerStmt body_stmt
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
  -- | function name
  Ident ->
  -- | function
  P.FunDef ext ->
  m (LoweredProc sizeT)
lowerFunDef
  with_ctrl
  fun_name
  fun@P.FunDef
    { P.param_types
    , P.ret_types
    , P.mbody
    } = withSandboxOf P._typingCtx $ do
    -- get the proc call that computes with garbage
    LoweredProc{lowered_def, aux_tys = g_aux_tys, out_tys = g_ret_tys} <- lowerFunDefWithGarbage fun_name fun
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
    let info_comment = printf "%sClean[%s]" (case with_ctrl of WithControl -> "Ctrl_"; _ -> "") fun_name

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

-- ================================================================================
-- Compiler
-- ================================================================================

class CompileU ext where
  compileU ::
    forall ext' m.
    ( m ~ CompilerT ext'
    , SizeType ext ~ SizeType ext'
    , PrecType ext ~ PrecType ext'
    ) =>
    ext ->
    [Ident] ->
    m (CQPL.UStmt (SizeType ext))

instance CompileU (P.Core size prec) where
  compileU = \case {}

class CompileU1 f where
  -- | all arguments/info provided to compile the given data
  type CompileArgs f ext

  -- | output of the compilation.
  type CompileResult f ext

  compileU1 ::
    forall ext m.
    ( CompileU ext
    , P.TypeInferrable ext (SizeType ext)
    , m ~ CompilerT ext
    ) =>
    CompileArgs f ext ->
    f ext ->
    m (CompileResult f ext)

instance CompileU1 P.Expr where
  type CompileArgs P.Expr ext = [Ident]
  type CompileResult P.Expr ext = (CQPL.UStmt (SizeType ext))

  compileU1 rets P.BasicExprE{basic_expr} = do
    let args = toList $ P.freeVars basic_expr
    return UnitaryS{qargs = args ++ rets, unitary = RevEmbedU args basic_expr}
  compileU1 rets P.RandomSampleE{distr_expr} = do
    rets' <- freshAux rets
    return $
      USeqS
        [ UnitaryS rets (DistrU distr_expr)
        , UnitaryS (rets ++ rets') COPY
        ]
  compileU1 rets P.FunCallE{fname, args} = do
    let uproc_id = mkUProcName fname
    ProcSignature{aux_tys} <- use (_procSignatures . at uproc_id) >>= maybeWithError "cannot find uproc signature"

    -- fresh aux each time.
    aux_vars <- mapM allocAncilla aux_tys
    let qargs = args ++ rets ++ aux_vars
    return UCallS{uproc_id, qargs, dagger = False}
  compileU1 rets P.PrimCallE{prim} = compileU prim rets
  compileU1 rets P.LoopE{initial_args, loop_body_fun} = error "TODO: compileU1 rets P.LoopE{initial_args, loop_body_fun}"

instance CompileU1 P.Stmt where
  type CompileArgs P.Stmt ext = ()
  type CompileResult P.Stmt ext = (CQPL.UStmt (SizeType ext))

  compileU1 () P.ExprS{rets, expr} = do
    -- compute result into a fresh set of vars, and swap at the end.
    tmp <- freshAux rets
    compute_expr <- compileU1 tmp expr
    return $ USeqS [compute_expr, UnitaryS{qargs = rets ++ tmp, unitary = SWAP}]
  compileU1 () P.IfThenElseS{cond, s_true, s_false} = do
    let out_t = toList $ P.outVars s_true
    tmp_t <- freshAux out_t

    let out_f = toList $ P.outVars s_true
    tmp_f <- freshAux out_f

    compiled_t <- compileU1 () s_true
    compiled_f <- compileU1 () s_false

    return $
      USeqS
        [ -- true branch:
          UnitaryS{qargs = out_t ++ tmp_t, unitary = COPY}
        , compiled_t
        , UnitaryS{qargs = out_t ++ tmp_t, unitary = SWAP}
        , -- false branch:
          UnitaryS{qargs = out_f ++ tmp_f, unitary = COPY}
        , compiled_f
        , -- when `cond` is true:
          -- - restore original false branch vars,
          -- - and then pull in the true branch results.
          UnitaryS{qargs = cond : out_f ++ tmp_f, unitary = Controlled SWAP}
        , UnitaryS{qargs = cond : out_t ++ tmp_t, unitary = Controlled SWAP}
        ]
  compileU1 () (P.SeqS ss) = CQPL.USeqS <$> mapM (compileU1 ()) ss

instance CompileU1 P.FunBody where
  type CompileArgs P.FunBody ext = ([P.VarType (SizeType ext)], [P.VarType (SizeType ext)])
  type CompileResult P.FunBody ext = (UProcBody (SizeType ext), [P.VarType (SizeType ext)])

  compileU1 (param_tys, _ret_tys) P.FunBody{param_names, ret_names, body_stmt} = do
    P._typingCtx .= Ctx.fromList (zip param_names param_tys)
    magnify P._funCtx . zoom P._typingCtx . ignoreWriter $ P.inferTypes body_stmt

    uproc_body_stmt <- compileU1 () body_stmt

    all_vars <- use $ P._typingCtx . to Ctx.toList
    P._typingCtx .= mempty
    let aux_vars = [(x, t) | (x, t) <- all_vars, x `notElem` param_names, x `notElem` ret_names]

    let uproc_body =
          UProcBody
            { uproc_param_names =
                param_names
                  ++ ret_names
                  ++ map fst aux_vars
            , uproc_param_tags =
                (CQPL.ParamInp <$ param_names)
                  ++ (CQPL.ParamOut <$ ret_names)
                  ++ (CQPL.ParamAux <$ aux_vars)
            , uproc_body_stmt
            }

    return (uproc_body, map snd aux_vars)

instance CompileU1 P.FunDef where
  type CompileArgs P.FunDef ext = Ident
  type CompileResult P.FunDef ext = (CQPL.ProcDef (SizeType ext), ProcSignature (SizeType ext))

  -- ext fn: compile as-is to ext uproc
  compileU1 proc_name P.FunDef{param_types, ret_types, mbody = Nothing} = do
    let info_comment = ""
    let proc_meta_params = []
    let proc_param_types = param_types ++ ret_types
    let proc_body = CQPL.ProcBodyU CQPL.UProcDecl
    let sign = ProcSignature{in_tys = param_types, out_tys = ret_types, aux_tys = []}
    pure (CQPL.ProcDef{..}, sign)

  -- fn: compile to uproc, and pass aux types.
  compileU1 proc_name P.FunDef{param_types, ret_types, mbody = Just body} = do
    let info_comment = ""
    let proc_meta_params = []

    (body', aux_tys) <- compileU1 (param_types, ret_types) body
    let proc_body = CQPL.ProcBodyU body'

    let proc_param_types = param_types ++ ret_types ++ aux_tys
    let sign = ProcSignature{in_tys = param_types, out_tys = ret_types, aux_tys}

    pure (CQPL.ProcDef{..}, sign)

instance CompileU1 P.NamedFunDef where
  type CompileArgs P.NamedFunDef ext = ()
  type CompileResult P.NamedFunDef ext = ()

  compileU1 () P.NamedFunDef{fun_name, fun_def} = do
    let uproc_name = mkUProcName fun_name
    (uproc, uproc_sign) <- compileU1 uproc_name fun_def

    addProc uproc
    _procSignatures . at uproc_name ?= uproc_sign

instance CompileU1 P.Program where
  type CompileArgs P.Program ext = ()
  type CompileResult P.Program ext = ()

  compileU1 () (P.Program fs) = mapM_ (compileU1 ()) fs

-- ================================================================================
-- Entry Point
-- ================================================================================

-- | Lower a full program into a unitary CQPL program.
lowerProgramU ::
  forall ext precT.
  ( Lowerable ext SizeT precT
  , CompileU ext
  , Show precT
  , Floating precT
  , P.HasFreeVars ext
  ) =>
  P.Program ext ->
  Either String (CQPL.Program SizeT)
lowerProgramU prog@(P.Program fs) = do
  unless (P.checkVarsUnique prog) $
    throwError "program does not have unique variables!"

  let config =
        default_
          & (P._funCtx .~ P.namedFunsToFunCtx fs)
  let ctx =
        default_
          & (_uniqNamesCtx .~ P.allNamesP prog)

  ((), _, outputU) <- runRWST (compileU1 () prog) config ctx
  let procs = outputU ^. _loweredProcs
  return $ CQPL.Program procs
