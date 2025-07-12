module Traq.UnitaryQPL.TypeCheck (
  typeCheckUStmt,
  typeCheckProc,
  typeCheckProgram,

  -- * Types
  CheckingCtx,
  TypeChecker,
  _procCtx,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.State (execStateT)
import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import qualified Traq.Data.Errors as Err

import Text.Printf (printf)
import Traq.Prelude
import Traq.ProtoLang (TypeCheckable (..), TypingCtx, VarType)
import qualified Traq.ProtoLang as P
import Traq.UnitaryQPL.Syntax

-- ================================================================================
-- Context
-- ================================================================================
data CheckingCtx holeT sizeT costT = CheckingCtx (ProcCtx holeT sizeT costT) (TypingCtx sizeT)

instance HasDefault (CheckingCtx holeT sizeT costT) where
  default_ = CheckingCtx default_ default_

type instance SizeType (CheckingCtx holeT sizeT costT) = sizeT
type instance CostType (CheckingCtx holeT sizeT costT) = costT

_procCtx :: Lens' (CheckingCtx holeT sizeT costT) (ProcCtx holeT sizeT costT)
_procCtx focus (CheckingCtx p t) = focus p <&> \p' -> CheckingCtx p' t

instance P.HasTypingCtx (CheckingCtx holeT sizeT costT) where
  _typingCtx focus (CheckingCtx p t) = focus t <&> CheckingCtx p

-- ================================================================================
-- Typechecker
-- ================================================================================
type TypeChecker holeT sizeT costT = MyReaderT (CheckingCtx holeT sizeT costT) (Either Err.MyError)

-- | Verify that the argument types match the deduced types
verifyArgTys :: (TypeCheckable sizeT) => [VarType sizeT] -> [VarType sizeT] -> TypeChecker holeT sizeT costT ()
verifyArgTys arg_tys tys = do
  when (length arg_tys /= length tys) $
    Err.throwErrorMessage $
      printf "mismatched number of args: inferred %d, actual %d" (length tys) (length arg_tys)

  when (arg_tys /= tys) $
    Err.throwErrorMessage $
      printf "mismatched args: inferred %s, actual %s" (show tys) (show arg_tys)

-- | Verify that the arguments match the deduced types
verifyArgs :: (TypeCheckable sizeT) => [Ident] -> [VarType sizeT] -> TypeChecker holeT sizeT costT ()
verifyArgs args tys = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ P._typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty
  verifyArgTys arg_tys tys

typeCheckUnitary :: forall holeT costT sizeT. (TypeCheckable sizeT) => Unitary sizeT -> [VarType sizeT] -> TypeChecker holeT sizeT costT ()
typeCheckUnitary Toffoli tys = verifyArgTys tys [tbool, tbool, tbool]
typeCheckUnitary CNOT tys = verifyArgTys tys [tbool, tbool]
typeCheckUnitary XGate tys = verifyArgTys tys [tbool]
typeCheckUnitary HGate tys = verifyArgTys tys [tbool]
typeCheckUnitary Unif _ = return ()
typeCheckUnitary Refl0 _ = return ()
typeCheckUnitary (RevEmbedU xs e) tys = do
  let in_tys = take (length xs) tys
  let gamma = Ctx.fromList $ zip xs in_tys
  -- TODO use separate context for metaparams
  gamma' <- execStateT ?? gamma $ do
    all_gamma <- view $ P._typingCtx . to Ctx.toList
    forM all_gamma $ \(x, ty) ->
      when (head x == '#') $
        Ctx.ins x .= ty
  let res = runReaderT (P.typeCheckBasicExpr e) gamma'
  case res of
    Left err -> Err.throwErrorMessage err
    Right ret_ty -> verifyArgTys (drop (length xs) tys) [ret_ty]
typeCheckUnitary (Controlled u) tys = do
  verifyArgTys [head tys] [tbool]
  typeCheckUnitary u (tail tys)
typeCheckUnitary (Adjoint u) tys = typeCheckUnitary u tys
typeCheckUnitary (LoadData f) tys = do
  proc_def <- view (_procCtx . Ctx.at f) >>= maybeWithError (Err.MessageE "cannot find function")
  let inferred_tys = proc_def ^.. to proc_params . traverse . _3
  verifyArgTys tys inferred_tys

typeCheckUStmt :: forall holeT sizeT costT. (Show holeT, TypeCheckable sizeT) => UStmt holeT sizeT -> TypeChecker holeT sizeT costT ()
-- single statements
typeCheckUStmt USkipS = return ()
typeCheckUStmt UHoleS{} = return ()
typeCheckUStmt (UCommentS _) = return ()
typeCheckUStmt UnitaryS{unitary, args} = do
  arg_tys <- forM args $ \x -> do
    mty <- view $ P._typingCtx . Ctx.at x
    maybeWithError (Err.MessageE $ printf "cannot find argument %s" x) mty
  typeCheckUnitary unitary arg_tys
typeCheckUStmt UCallS{proc_id, args} = do
  proc_param_tys <-
    view $
      _procCtx
        . Ctx.at proc_id
        . singular _Just
        . to proc_params
        . to (map $ view _3)
  verifyArgs args proc_param_tys
-- compound statements
typeCheckUStmt (USeqS ss) = mapM_ typeCheckUStmt' ss
typeCheckUStmt (URepeatS _ body) = typeCheckUStmt' body
typeCheckUStmt UForInRangeS{iter_meta_var, iter_lim, loop_body} = do
  let iter_lim_ty = case iter_lim of
        P.MetaSize n -> P.Fin n
        _ -> error "unsupported loop limit"
  local (P._typingCtx . Ctx.ins ('#' : iter_meta_var) .~ iter_lim_ty) $ do
    typeCheckUStmt' loop_body
typeCheckUStmt UWithComputedS{with_stmt, body_stmt} = mapM_ typeCheckUStmt' [with_stmt, body_stmt]

typeCheckUStmt' :: (Show holeT, TypeCheckable sizeT) => UStmt holeT sizeT -> TypeChecker holeT sizeT costT ()
typeCheckUStmt' s = do
  gamma <- view P._typingCtx
  typeCheckUStmt s
    `throwFrom` Err.MessageE (printf "with context: %s" (show gamma))
    `throwFrom` Err.MessageE (printf "typecheck failed: %s" (show s))

typeCheckProc :: (Show holeT, TypeCheckable sizeT, Show costT) => ProcDef holeT sizeT costT -> TypeChecker holeT sizeT costT ()
-- definition with a body
typeCheckProc procdef@UProcDef{proc_params, proc_body_or_tick = Right proc_body} =
  local (P._typingCtx .~ Ctx.fromList (proc_params & each %~ withoutTag)) $ do
    gamma <- view P._typingCtx
    typeCheckUStmt' proc_body
      `throwFrom` Err.MessageE (printf "with context: %s" (show gamma))
      `throwFrom` Err.MessageE (printf "typecheck proc failed: %s" (show procdef))
 where
  withoutTag (x, _, ty) = (x, ty)
-- declaration with a tick
typeCheckProc UProcDef{proc_body_or_tick = Left _} = return ()

typeCheckProgram :: (Show holeT, TypeCheckable sizeT, Show costT) => Program holeT sizeT costT -> Either Err.MyError ()
typeCheckProgram Program{proc_defs} = do
  let ctx = default_ & (_procCtx .~ proc_defs)
  forM_ proc_defs $ (runMyReaderT ?? ctx) . typeCheckProc
