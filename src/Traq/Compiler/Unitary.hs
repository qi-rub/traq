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
  withTag,

  -- * Internal
  compileU1,
) where

import Control.Monad (zipWithM)
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (Foldable (toList))

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis.Annotate.Prelude as A
import qualified Traq.CPL as CPL
import qualified Traq.CQPL as CQPL
import Traq.CQPL.Syntax
import Traq.Compiler.Prelude
import Traq.Prelude

-- ================================================================================
-- Helpers
-- ================================================================================

-- | Allocate an ancilla register, and update the typing context.
allocAncillaWithPref :: (size ~ SizeType ext) => Ident -> CPL.VarType size -> CompilerT ext Ident
allocAncillaWithPref pref ty = do
  name <- newIdent pref
  zoom CPL._typingCtx $ Ctx.put name ty
  return name

-- | Allocate an ancilla register @aux_<<n>>@, and update the typing context.
allocAncilla :: (size ~ SizeType ext) => CPL.VarType size -> CompilerT ext Ident
allocAncilla = allocAncillaWithPref "aux"

-- | Allocate fresh set of auxiliaries corresponding to the types of given vars.
freshAux :: (m ~ CompilerT ext) => [Ident] -> m [Ident]
freshAux xs = do
  tys <- zoom CPL._typingCtx $ mapM Ctx.lookup xs
  zipWithM allocAncillaWithPref xs tys

withTag :: ParamTag -> [(Ident, CPL.VarType a)] -> [(Ident, ParamTag, CPL.VarType a)]
withTag tag = map $ \(x, ty) -> (x, tag, ty)

-- ================================================================================
-- Compiler
-- ================================================================================

class (CPL.TypeInferrable ext (SizeType ext)) => CompileU ext where
  compileU ::
    forall ext' m.
    ( m ~ CompilerT ext'
    , SizeType ext ~ SizeType ext'
    , PrecType ext ~ PrecType ext'
    ) =>
    ext ->
    [Ident] ->
    m (CQPL.UStmt (SizeType ext))

instance (CPL.TypingReqs size) => CompileU (CPL.Core size prec) where
  compileU = \case {}

instance (CPL.TypingReqs size) => CompileU (A.AnnFailProb (CPL.Core size prec)) where
  compileU (A.AnnFailProb _ ext) = case ext of {}

class CompileU1 f where
  -- | all arguments/info provided to compile the given data
  type CompileArgs f ext

  -- | output of the compilation.
  type CompileResult f ext

  compileU1 ::
    forall ext m.
    ( CompileU ext
    , CPL.TypeInferrable ext (SizeType ext)
    , m ~ CompilerT ext
    ) =>
    CompileArgs f ext ->
    f ext ->
    m (CompileResult f ext)

instance CompileU1 CPL.Expr where
  type CompileArgs CPL.Expr ext = [Ident]
  type CompileResult CPL.Expr ext = (CQPL.UStmt (SizeType ext))

  compileU1 rets CPL.BasicExprE{basic_expr} = do
    let args = toList $ CPL.freeVars basic_expr
    return UnitaryS{qargs = map Arg args ++ map Arg rets, unitary = RevEmbedU args basic_expr}
  compileU1 rets CPL.RandomSampleE{distr_expr} = do
    rets' <- freshAux rets
    return $
      USeqS
        [ UnitaryS (map Arg rets) (DistrU distr_expr)
        , UnitaryS (map Arg (rets ++ rets')) (BasicGateU COPY)
        ]
  compileU1 rets CPL.FunCallE{fname, args} = do
    let uproc_id = mkUProcName fname
    ProcSignature{aux_tys} <- use (_procSignatures . at uproc_id) >>= maybeWithError "cannot find uproc signature"

    -- fresh aux each time.
    aux_vars <- mapM allocAncilla aux_tys
    let qargs = map Arg $ args ++ rets ++ aux_vars
    return UCallS{uproc_id, qargs, dagger = False}
  compileU1 rets CPL.PrimCallE{prim} = compileU prim rets
  compileU1 rets CPL.LoopE{initial_args, loop_body_fun} = do
    CPL.FunDef{param_types, ret_types} <- view (CPL._funCtx . Ctx.at loop_body_fun) >>= maybeWithError "cannot find loop body fun"
    n <- case last param_types of
      CPL.Fin n -> pure n
      _ -> throwError "loop index must be of type `Fin`"

    let uproc_id = mkUProcName loop_body_fun
    ProcSignature{aux_tys} <- use (_procSignatures . at uproc_id) >>= maybeWithError "cannot find uproc signature"

    -- fresh aux for each iteration
    aux_vars <- mapM (allocAncilla . CPL.Arr n) aux_tys

    iter_meta_var <- newIdent "ITER"
    iter_vars <- allocAncilla (CPL.Arr n (CPL.Fin n))

    intermediates <- mapM (allocAncilla . CPL.Arr (n + 1)) ret_types

    let at_ix x = ArrElemArg (Arg x) (CPL.MetaName iter_meta_var)

    return $
      USeqS $
        [ UnitaryS
            [Arg x_in, ArrElemArg (Arg x_out) (MetaSize 0)]
            (BasicGateU COPY)
        | (x_out, x_in) <- zip intermediates initial_args
        ]
          ++ [ UForInRangeS
                 { iter_meta_var
                 , iter_lim = CPL.MetaSize n
                 , uloop_body =
                     USeqS
                       [ UCallS
                           { uproc_id = uproc_id
                           , dagger = False
                           , qargs =
                               map at_ix intermediates
                                 ++ [at_ix iter_vars]
                                 ++ map at_ix intermediates
                                 ++ map at_ix aux_vars
                           }
                       ]
                 , dagger = False
                 }
             ]
          ++ [ UnitaryS
                 [ArrElemArg (Arg x_last) (MetaSize n), Arg x_ret]
                 (BasicGateU COPY)
             | (x_ret, x_last) <- zip rets intermediates
             ]

instance CompileU1 CPL.Stmt where
  type CompileArgs CPL.Stmt ext = ()
  type CompileResult CPL.Stmt ext = (CQPL.UStmt (SizeType ext))

  compileU1 () CPL.ExprS{rets, expr} = do
    -- compute result into a fresh set of vars, and swap at the end.
    tmp <- freshAux rets
    compute_expr <- compileU1 tmp expr
    return $ USeqS [compute_expr, UnitaryS{qargs = map Arg (rets ++ tmp), unitary = BasicGateU SWAP}]
  compileU1 () CPL.IfThenElseS{cond, s_true, s_false} = do
    let out_t = toList $ CPL.outVars s_true
    tmp_t <- freshAux out_t

    let out_f = toList $ CPL.outVars s_true
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
  compileU1 () (CPL.SeqS ss) = CQPL.USeqS <$> mapM (compileU1 ()) ss

instance CompileU1 CPL.FunBody where
  type CompileArgs CPL.FunBody ext = ([CPL.VarType (SizeType ext)], [CPL.VarType (SizeType ext)])
  type CompileResult CPL.FunBody ext = (UProcBody (SizeType ext), [CPL.VarType (SizeType ext)])

  compileU1 (param_tys, _ret_tys) CPL.FunBody{param_names, ret_names, body_stmt} = do
    CPL._typingCtx .= Ctx.fromList (zip param_names param_tys)
    magnify CPL._funCtx . zoom CPL._typingCtx . ignoreWriter $ CPL.inferTypes body_stmt

    uproc_body_stmt <- compileU1 () body_stmt

    all_vars <- use $ CPL._typingCtx . to Ctx.toList
    CPL._typingCtx .= mempty
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

instance CompileU1 CPL.FunDef where
  type CompileArgs CPL.FunDef ext = Ident
  type CompileResult CPL.FunDef ext = (CQPL.ProcDef (SizeType ext), ProcSignature (SizeType ext))

  -- ext fn: compile as-is to ext uproc
  compileU1 proc_name CPL.FunDef{param_types, ret_types, mbody = Nothing} = do
    let info_comment = ""
    let proc_meta_params = []
    let proc_param_types = param_types ++ ret_types
    let proc_body = CQPL.ProcBodyU CQPL.UProcDecl
    let sign = ProcSignature{in_tys = param_types, out_tys = ret_types, aux_tys = []}
    pure (CQPL.ProcDef{..}, sign)

  -- fn: compile to uproc, and pass aux types.
  compileU1 proc_name CPL.FunDef{param_types, ret_types, mbody = Just body} = do
    let info_comment = ""
    let proc_meta_params = []

    (body', aux_tys) <- compileU1 (param_types, ret_types) body
    let proc_body = CQPL.ProcBodyU body'

    let proc_param_types = param_types ++ ret_types ++ aux_tys
    let sign = ProcSignature{in_tys = param_types, out_tys = ret_types, aux_tys}

    pure (CQPL.ProcDef{..}, sign)

instance CompileU1 CPL.NamedFunDef where
  type CompileArgs CPL.NamedFunDef ext = ()
  type CompileResult CPL.NamedFunDef ext = ()

  compileU1 () CPL.NamedFunDef{fun_name, fun_def} = do
    let uproc_name = mkUProcName fun_name
    (uproc, uproc_sign) <- compileU1 uproc_name fun_def

    addProc uproc
    _procSignatures . at uproc_name ?= uproc_sign

instance CompileU1 CPL.Program where
  type CompileArgs CPL.Program ext = ()
  type CompileResult CPL.Program ext = ()

  compileU1 () (CPL.Program fs) = mapM_ (compileU1 ()) fs

-- ================================================================================
-- Entry Point
-- ================================================================================

-- | Lower a full program into a unitary CQPL program.
lowerProgramU ::
  forall ext size prec.
  ( CompileU ext
  , Show prec
  , Floating prec
  , CPL.HasFreeVars ext
  , prec ~ PrecType ext
  , size ~ SizeType ext
  , CPL.TypeInferrable ext size
  ) =>
  CPL.Program ext ->
  Either String (CQPL.Program size)
lowerProgramU = compileWith (compileU1 ())
