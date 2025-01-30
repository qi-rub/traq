module QCompose.ProtoLang.TypeCheck where

import           Control.Monad             (forM_, unless, when)
import qualified Data.Map                  as M
import           QCompose.ProtoLang.Syntax

type TypingCtx = M.Map Ident VarType

data OracleDecl = OracleDecl [VarType] [VarType] -- arg_types ret_types

typeCheckProg :: FunCtx -> OracleDecl -> Either String ()
typeCheckProg fns (OracleDecl o_arg_tys o_ret_tys) = mapM_ checkFun (M.keys fns)
  where
    sbool = Fin 2

    checkFun :: Ident -> Either String ()
    checkFun f = do
      let FunDef fn_args fn_rets body = fns M.! f
      let gamma = M.fromList fn_args
      gamma' <- checkStmt body gamma
      forM_ fn_rets $ \(x, t) -> do
        let t' = gamma' M.! x
        when (t /= t') $ Left $ "return term " <> x <> ": expected type " <> show t <> ", got type " <> show t'

    checkNotPresent :: [Ident] -> TypingCtx -> Either String ()
    checkNotPresent xs gamma = forM_ xs $ \x -> do
      when (M.member x gamma) $ Left ("variable " <> x <> " already defined: " <> show gamma)

    checkPresent :: [Ident] -> TypingCtx -> Either String ()
    checkPresent xs gamma = forM_ xs $ \x -> do
      unless (M.member x gamma) $ Left ("variable " <> x <> " not found: " <> show gamma)

    checkInOuts :: TypingCtx -> [Ident] -> [Ident] -> Either String ()
    checkInOuts gamma ins outs = checkPresent ins gamma >> checkNotPresent outs gamma

    checkStmt :: Stmt -> TypingCtx -> Either String TypingCtx
    checkStmt s gamma = do
      outs <- checkStmt' s gamma
      return $ M.union (M.fromList outs) gamma

    checkStmt' :: Stmt -> TypingCtx -> Either String [(Ident, VarType)]
    -- x <- x'
    checkStmt' (SAssign x x') gamma = do
      checkInOuts gamma [x'] [x]
      return [(x, gamma M.! x')]

    -- x <- v : t
    checkStmt' (SConst x _ t) gamma = do
      checkInOuts gamma [] [x]
      checkNotPresent [x] gamma
      return [(x, t)]

    -- x <- op a
    checkStmt' (SUnOp x op a) gamma = do
      checkInOuts gamma [a] [x]
      let t = gamma M.! a
      case op of
        PNot -> when (t /= sbool) $ Left ("`not` requires bool, got " <> show t)
      return [(x, sbool)]

    -- x <- a op b
    checkStmt' (SBinOp x op a b) gamma = do
      checkInOuts gamma [a, b] [x]
      let ta = gamma M.! a
      let tb = gamma M.! b
      case op of
        PAnd -> unless (ta == sbool && tb == sbool) $ Left ("`and` requires bools, got " <> show [ta, tb])
        _ -> return ()

      let Fin na = ta
      let Fin nb = tb
      let t = case op of
            PAnd -> sbool
            PLeq -> sbool
            PAdd -> Fin $ max na nb
      return [(x, t)]

    -- ret <- Oracle(args)
    checkStmt' (SOracle ret args) gamma = do
      checkInOuts gamma args ret

      let arg_tys = map (gamma M.!) args
      when (arg_tys /= o_arg_tys) $
        Left ("oracle expects " <> show o_arg_tys <> ", got " <> show args)

      when (length ret /= length o_ret_tys) $
        Left ("oracle returns " <> show (length o_ret_tys) <> " values, but RHS has " <> show ret)

      return $ zip ret o_ret_tys

    -- ret <- f(args)
    checkStmt' (SFunCall ret f args) gamma = do
      checkInOuts gamma args ret

      let FunDef fn_params fn_rets _ = fns M.! f

      let arg_tys = map (gamma M.!) args
      let param_tys = map snd fn_params
      when (arg_tys /= param_tys) $
        Left (f <> " expects " <> show param_tys <> ", got " <> show (zip args arg_tys))

      when (length ret /= length fn_rets) $
        Left ("oracle returns " <> show (length fn_rets) <> " values, but RHS has " <> show ret)

      return $ zip ret (map snd fn_rets)

    -- x, ok <- search[f](args)
    checkStmt' (SSearch x ok f args) gamma = do
      checkInOuts gamma args [x, ok]

      let FunDef fn_params fn_rets _ = fns M.! f

      when (map snd fn_rets /= [sbool]) $
        Left ("predicate " <> f <> " should return bool, got " <> show fn_rets)

      let arg_tys = map (gamma M.!) args
      let param_tys = map snd fn_params
      when (arg_tys /= init param_tys) $
        Left ("predicate " <> f <> " expects " <> show (init param_tys) <> ", got " <> show (zip args arg_tys))

      return [(ok, sbool), (x, last param_tys)]

    -- ok <- contains[f](args)
    checkStmt' (SContains ok f args) gamma = do
      checkInOuts gamma args [ok]

      let FunDef fn_params fn_rets _ = fns M.! f

      when (map snd fn_rets /= [sbool]) $
        Left ("predicate " <> f <> " should return bool, got " <> show fn_rets)

      let arg_tys = map (gamma M.!) args
      let param_tys = map snd fn_params
      when (arg_tys /= init param_tys) $
        Left ("predicate " <> f <> " expects " <> show (init param_tys) <> ", got " <> show (zip args arg_tys))

      return [(ok, sbool)]

    -- if b then s_t else s_f
    checkStmt' (SIfTE b s_t s_f) gamma = do
      checkInOuts gamma [b] []
      outs_t <- checkStmt' s_t gamma
      outs_f <- checkStmt' s_f gamma
      when (outs_t /= outs_f) $ Left ("if: branches must declare same variables, got " <> show [outs_t, outs_f])
      return outs_t

    -- s_1 ; s_2
    checkStmt' (SSeq s_1 s_2) gamma = checkStmt s_1 gamma >>= checkStmt' s_2
