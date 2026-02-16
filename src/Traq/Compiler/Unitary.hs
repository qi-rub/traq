{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Traq.Compiler.Unitary (
  lowerProgramU,

  -- * Class
  CompileU (..),

  -- ** Helpers
  allocAncillaWithPref,
  allocAncilla,
  ControlFlag (..),
  withTag,

  -- * Internal
  compileU1,
) where

import Control.Monad (zipWithM)
import Data.Foldable (Foldable (toList))

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis.Annotate.Prelude as A
import qualified Traq.CQPL as CQPL
import Traq.CQPL.Syntax
import Traq.Compiler.Prelude
import Traq.Prelude
import qualified Traq.ProtoLang as P

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

data ControlFlag = WithControl | WithoutControl deriving (Eq, Show, Read, Enum)

withTag :: ParamTag -> [(Ident, P.VarType a)] -> [(Ident, ParamTag, P.VarType a)]
withTag tag = map $ \(x, ty) -> (x, tag, ty)

-- ================================================================================
-- Compiler
-- ================================================================================

class (P.TypeInferrable ext (SizeType ext)) => CompileU ext where
  compileU ::
    forall ext' m.
    ( m ~ CompilerT ext'
    , SizeType ext ~ SizeType ext'
    , PrecType ext ~ PrecType ext'
    ) =>
    ext ->
    [Ident] ->
    m (CQPL.UStmt (SizeType ext))

instance (P.TypingReqs size) => CompileU (P.Core size prec) where
  compileU = \case {}

instance (P.TypingReqs size) => CompileU (A.AnnFailProb (P.Core size prec)) where
  compileU (A.AnnFailProb _ ext) = case ext of {}

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
    return UnitaryS{qargs = map Arg args ++ map Arg rets, unitary = RevEmbedU args basic_expr}
  compileU1 rets P.RandomSampleE{distr_expr} = do
    rets' <- freshAux rets
    return $
      USeqS
        [ UnitaryS (map Arg rets) (DistrU distr_expr)
        , UnitaryS (map Arg (rets ++ rets')) (BasicGateU COPY)
        ]
  compileU1 rets P.FunCallE{fname, args} = do
    let uproc_id = mkUProcName fname
    ProcSignature{aux_tys} <- use (_procSignatures . at uproc_id) >>= maybeWithError "cannot find uproc signature"

    -- fresh aux each time.
    aux_vars <- mapM allocAncilla aux_tys
    let qargs = map Arg $ args ++ rets ++ aux_vars
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
    return $ USeqS [compute_expr, UnitaryS{qargs = map Arg (rets ++ tmp), unitary = BasicGateU SWAP}]
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
          UnitaryS{qargs = map Arg (out_t ++ tmp_t), unitary = BasicGateU COPY}
        , compiled_t
        , UnitaryS{qargs = map Arg (out_t ++ tmp_t), unitary = BasicGateU SWAP}
        , -- false branch:
          UnitaryS{qargs = map Arg (out_f ++ tmp_f), unitary = BasicGateU COPY}
        , compiled_f
        , -- when `cond` is true:
          -- - restore original false branch vars,
          -- - and then pull in the true branch results.
          UnitaryS{qargs = map Arg (cond : out_f ++ tmp_f), unitary = Controlled (BasicGateU SWAP)}
        , UnitaryS{qargs = map Arg (cond : out_t ++ tmp_t), unitary = Controlled (BasicGateU SWAP)}
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
  forall ext size prec.
  ( CompileU ext
  , Show prec
  , Floating prec
  , P.HasFreeVars ext
  , prec ~ PrecType ext
  , size ~ SizeType ext
  , P.TypeInferrable ext size
  ) =>
  P.Program ext ->
  Either String (CQPL.Program size)
lowerProgramU = compileWith (compileU1 ())
