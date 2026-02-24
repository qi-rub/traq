{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Quantum Max Finding.

References:

 1. [Quantifying Grover speed-ups beyond asymptotic analysis](https://arxiv.org/abs/2203.04975)
-}
module Traq.Primitives.Max.QMax (
  -- * Primitive
  QMax (..),

  -- * Formulas
  _EQMax,
  _WQMax,
) where

import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis as A
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QMax size prec = QMax {arg_ty, res_ty :: P.VarType size}
  deriving (Eq, Show, Read)

type instance SizeType (QMax size prec) = size
type instance PrecType (QMax size prec) = prec

newtype QMaxFunArg a = QMaxFunArg a

type instance PrimFnShape (QMax size prec) = QMaxFunArg

instance ValidPrimShape QMaxFunArg where
  listToShape [fun] = Right $ QMaxFunArg fun
  listToShape _ = Left "max expects exactly one function argument"

  shapeToList (QMaxFunArg fun) = [fun]

instance P.MapSize (QMax size prec) where
  type MappedSize (QMax size prec) size' = QMax size' prec

  mapSize f (QMax t r) = QMax (P.mapSize f t) (P.mapSize f r)

instance (Show size) => SerializePrim (QMax size prec) where
  primNames = ["max"]
  parsePrimParams tp _ = QMax <$> P.varType tp <*> (comma tp >> P.varType tp)
  printPrimParams QMax{arg_ty, res_ty} = [PP.toCodeString arg_ty, PP.toCodeString res_ty]

-- Type check
instance (Eq size) => TypeCheckPrim (QMax size prec) size where
  inferRetTypesPrim QMax{arg_ty} (QMaxFunArg fun_type) = do
    let P.FnType param_types ret_types = fun_type

    when (param_types /= [arg_ty]) $
      throwError "max: argument does not match specified type."

    res_ty <- case ret_types of
      [P.Fin n] -> pure $ P.Fin n
      _ ->
        throwError $
          printf "`max` fun arg must return a single value, got %d values" (length ret_types)

    return [res_ty, arg_ty]

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance EvalPrim (QMax size prec) size prec where
  evalPrim QMax{arg_ty} (QMaxFunArg fun_eval) = do
    let search_range = P.domain arg_ty

    vs <- forM search_range $ \val -> do
      res <- fun_eval [val]
      case res of
        [P.FinV v] -> return v
        _ -> error "fail"

    return [P.FinV $ maximum vs]

-- ================================================================================
-- Unitary
-- ================================================================================

instance
  (Integral size, Floating prec, A.SizeToPrec size prec) =>
  UnitaryCostPrim (QMax size prec) size prec
  where
  unitaryQueryCosts QMax{arg_ty} _ = QMaxFunArg (weakQueries (A.sizeToPrec _N))
   where
    _N = P.domainSize arg_ty

  unitaryExprCosts _ _ = Alg.zero

instance (P.TypingReqs size, Integral size, RealFloat prec, Show prec) => UnitaryCompilePrim (QMax size prec) size prec where
  compileUPrim QMax{arg_ty, res_ty} _ = do
    -- Return variables and their types
    (res_var, argmax_var) <-
      view (to ret_vars) >>= \case
        [x, y] -> pure (x, y)
        _ -> throwError "typecheck failed"

    -- Function argument: unitary call builder and aux types
    QMaxFunArg call_ufun <- view $ to mk_ucall
    QMaxFunArg fun_aux_tys <- view $ to uproc_aux_types

    Compiler.buildUProc "UMax" [] [(res_var, res_ty), (argmax_var, arg_ty)] $ do
      let _N = P.domainSize arg_ty
      inp <- Compiler.allocLocalWithPrefix "inp" $ P.Arr _N arg_ty
      oup <- Compiler.allocLocalWithPrefix "out" $ P.Arr _N res_ty
      aux <- mapM (Compiler.allocLocal . P.Arr _N) fun_aux_tys

      i <- Compiler.newIdent "x"
      Compiler.withUStmt (CQPL.UForInDomainS i arg_ty False) $ do
        let inp_ix = CQPL.ArrElemArg (CQPL.Arg inp) (P.MetaName i)
        let oup_ix = CQPL.ArrElemArg (CQPL.Arg oup) (P.MetaName i)
        let aux_ix = map ((`CQPL.ArrElemArg` P.MetaName i) . CQPL.Arg) aux

        Compiler.addUStmt $ call_ufun (inp_ix : oup_ix : aux_ix)

      Compiler.addUStmt $ CQPL.UCommentS $ printf "unitarily compute: %s := max(%s); %s := argmax(%s);" res_var oup argmax_var oup

-- ================================================================================
-- Quantum
-- ================================================================================

-- [1], Page 16, below Eq. 11
_EQMax :: forall size prec. (Floating prec, A.SizeToPrec size prec) => size -> prec
_EQMax n = 6.3505 * sqrt_n + 2.8203
 where
  sqrt_n :: prec
  sqrt_n = sqrt $ A.sizeToPrec n

-- [1], Corollary 1.
_WQMax :: forall size prec. (Floating prec, A.SizeToPrec size prec) => size -> A.FailProb prec -> prec
_WQMax n eps = 3 * _EQMax n * log_eps
 where
  log_eps :: prec
  log_eps = log (1 / A.getFailProb eps)

instance
  (Integral size, Floating prec, A.SizeToPrec size prec) =>
  QuantumHavocCostPrim (QMax size prec) size prec
  where
  quantumQueryCostsUnitary QMax{arg_ty} eps = QMaxFunArg (strongQueries $ _WQMax _N eps)
   where
    _N = P.domainSize arg_ty

  -- no quantum queries
  quantumQueryCostsQuantum _ _ = QMaxFunArg 0

  quantumExprCosts = Alg.zero

instance
  (Floating prec, Integral size, A.SizeToPrec size prec) =>
  QuantumExpCostPrim (QMax size prec) size prec
  where
  quantumExpQueryCostsUnitary QMax{arg_ty} _ _ = QMaxFunArg (strongQueries $ _EQMax _N)
   where
    _N = P.domainSize arg_ty

  -- no quantum queries
  quantumExpQueryCostsQuantum _ _ _ = QMaxFunArg []

  quantumExpExprCosts = Alg.zero

instance (P.TypingReqs size, Integral size, RealFloat prec, Show prec, A.SizeToPrec size prec) => QuantumCompilePrim (QMax size prec) size prec where
  compileQPrim QMax{arg_ty, res_ty} eps = do
    -- Return variables and their types
    (res_var, argmax_var) <-
      view (to ret_vars) >>= \case
        [x, y] -> pure (x, y)
        _ -> throwError "typecheck failed"

    -- Build cmp :: (res_ty, arg_ty) -> Bool
    cmp <- do
      x <- Compiler.newIdent "x"
      prev <- Compiler.newIdent "y"
      b <- Compiler.newIdent "b"
      cmp <- Compiler.buildUProc "Compare" [] [(prev, res_ty), (x, arg_ty), (b, P.tbool)] $ do
        -- f :: arg_ty -> res_ty
        QMaxFunArg call_ufun <- lift $ view $ to mk_ucall
        QMaxFunArg aux_tys <- lift $ view $ to uproc_aux_types

        out <- Compiler.allocLocalWithPrefix "out" res_ty
        aux <- mapM Compiler.allocLocal aux_tys
        Compiler.addUStmt $ call_ufun $ map CQPL.Arg (x : out : aux)

        Compiler.addUStmt $
          CQPL.UnitaryS [CQPL.Arg prev, CQPL.Arg out, CQPL.Arg b] $
            CQPL.RevEmbedU ["a", "b"] $
              P.BinOpE P.LtOp (P.VarE "a") (P.VarE "b")

      Compiler.addProc cmp
      return cmp

    let _N = P.domainSize arg_ty
    let max_queries = ceiling (_WQMax _N eps) :: size
    let fuel_ty = P.Fin max_queries

    -- Build: qsearch with bounded fuel
    qsearch <- do
      p <- Compiler.buildProc "QSearch" [] [("fuel", fuel_ty), ("y", res_ty), ("x", arg_ty)] $ do
        Compiler.addStmt $ CQPL.CommentS "TODO: QSearch body"

      Compiler.addProc p
      return p

    -- Build the main algorithm
    Compiler.buildProc "QMax" [] [(res_var, res_ty), (argmax_var, arg_ty)] $ do
      fuel <- Compiler.allocLocalWithPrefix "fuel" fuel_ty
      Compiler.addStmt $ CQPL.AssignS [fuel] (P.ConstE (P.FinV $ max_queries - 1) fuel_ty)

      -- choose x \in arg_ty uniformly at random
      let x = argmax_var
      Compiler.addStmt $ CQPL.RandomS [x] (P.UniformE arg_ty)

      -- set y <- f(x);
      let y = res_var
      QMaxFunArg call_fun <- lift $ view $ to mk_call
      Compiler.addStmt $ call_fun [CQPL.Arg x, CQPL.Arg y]

      Compiler.withStmt (CQPL.RepeatS (P.MetaSize max_queries)) $ do
        Compiler.addStmt $ CQPL.CallS (CQPL.FunctionCall (CQPL.proc_name qsearch)) [] [CQPL.Arg fuel, CQPL.Arg y, CQPL.Arg x]
