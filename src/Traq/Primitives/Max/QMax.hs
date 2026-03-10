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

import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import qualified Traq.CQPL as CQPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.QSearchCFNW (groverK)
import qualified Traq.Utils.Printing as PP

-- ================================================================================
-- Primitive Class Implementation
-- ================================================================================

data QMax size prec = QMax {arg_ty, res_ty :: CPL.VarType size}
  deriving (Eq, Show, Read)

type instance SizeType (QMax size prec) = size
type instance PrecType (QMax size prec) = prec

newtype QMaxFunArg a = QMaxFunArg a

type instance PrimFnShape (QMax size prec) = QMaxFunArg

instance ValidPrimShape QMaxFunArg where
  listToShape [fun] = Right $ QMaxFunArg fun
  listToShape _ = Left "max expects exactly one function argument"

  shapeToList (QMaxFunArg fun) = [fun]

instance CPL.MapSize (QMax size prec) where
  type MappedSize (QMax size prec) size' = QMax size' prec

  mapSize f (QMax t r) = QMax (CPL.mapSize f t) (CPL.mapSize f r)

instance (Show size) => SerializePrim (QMax size prec) where
  primNames = ["max"]
  parsePrimParams tp _ = QMax <$> CPL.varType tp <*> (comma tp >> CPL.varType tp)
  printPrimParams QMax{arg_ty, res_ty} = [PP.toCodeString arg_ty, PP.toCodeString res_ty]

-- Type check
instance (Eq size) => TypeCheckPrim (QMax size prec) size where
  inferRetTypesPrim QMax{arg_ty} (QMaxFunArg fun_type) = do
    let CPL.FnType param_types ret_types = fun_type

    when (param_types /= [arg_ty]) $
      throwError "max: argument does not match specified type."

    res_ty <- case ret_types of
      [CPL.Fin n] -> pure $ CPL.Fin n
      _ ->
        throwError $
          printf "`max` fun arg must return a single value, got %d values" (length ret_types)

    return [res_ty, arg_ty]

{- | Evaluate an `any` call by evaluating the predicate on each element of the search space
 and or-ing the results.
-}
instance EvalPrim (QMax size prec) size prec where
  evalPrim QMax{arg_ty} (QMaxFunArg fun_eval) = do
    let search_range = CPL.domain arg_ty

    vs <- forM search_range $ \val -> do
      res <- fun_eval [val]
      case res of
        [CPL.FinV v] -> return v
        _ -> error "fail"

    return [CPL.FinV $ maximum vs]

-- ================================================================================
-- Unitary
-- ================================================================================

instance
  (Integral size, Floating prec, A.SizeToPrec size prec) =>
  UnitaryCostPrim (QMax size prec) size prec
  where
  unitaryQueryCosts QMax{arg_ty} _ = QMaxFunArg (weakQueries (A.sizeToPrec _N))
   where
    _N = CPL.domainSize arg_ty

  unitaryExprCosts _ _ = Alg.zero

instance (CPL.TypingReqs size, Integral size, RealFloat prec, Show prec) => UnitaryCompilePrim (QMax size prec) size prec where
  compileUPrim QMax{arg_ty, res_ty} _ = do
    -- Return variables
    res_var <- Compiler.newIdent "ret"
    argmax_var <- Compiler.newIdent "ret"

    -- Function argument: unitary call builder and aux types
    QMaxFunArg call_ufun <- view $ to mk_ucall
    QMaxFunArg fun_aux_tys <- view $ to uproc_aux_types

    Compiler.buildUProc "UMax" [] [(res_var, res_ty), (argmax_var, arg_ty)] $ do
      let _N = CPL.domainSize arg_ty
      inp <- Compiler.allocLocalWithPrefix "inp" $ CPL.Arr _N arg_ty
      oup <- Compiler.allocLocalWithPrefix "out" $ CPL.Arr _N res_ty
      aux <- mapM (Compiler.allocLocal . CPL.Arr _N) fun_aux_tys

      i <- Compiler.newIdent "x"
      Compiler.withUStmt (CQPL.UForInDomainS i arg_ty False) $ do
        let inp_ix = CQPL.ArrElemArg (CQPL.Arg inp) (CPL.MetaName i)
        let oup_ix = CQPL.ArrElemArg (CQPL.Arg oup) (CPL.MetaName i)
        let aux_ix = map ((`CQPL.ArrElemArg` CPL.MetaName i) . CQPL.Arg) aux

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
    _N = CPL.domainSize arg_ty

  -- no quantum queries
  quantumQueryCostsQuantum _ _ = QMaxFunArg 0

  quantumExprCosts = Alg.zero

instance
  (Floating prec, Integral size, A.SizeToPrec size prec) =>
  QuantumExpCostPrim (QMax size prec) size prec
  where
  quantumExpQueryCostsUnitary QMax{arg_ty} _ _ = QMaxFunArg (strongQueries $ _EQMax _N)
   where
    _N = CPL.domainSize arg_ty

  -- no quantum queries
  quantumExpQueryCostsQuantum _ _ _ = QMaxFunArg []

  quantumExpExprCosts = Alg.zero

instance (CPL.TypingReqs size, Integral size, RealFloat prec, Show prec, A.SizeToPrec size prec) => QuantumCompilePrim (QMax size prec) size prec where
  compileQPrim QMax{arg_ty, res_ty} eps = do
    -- Return variables
    res_var <- Compiler.newIdent "ret"
    argmax_var <- Compiler.newIdent "ret"

    -- Build cmp :: (res_ty, arg_ty) -> Bool
    cmp <- do
      x <- Compiler.newIdent "x"
      prev <- Compiler.newIdent "y"
      b <- Compiler.newIdent "b"
      cmp <- Compiler.buildUProc "Compare" [] [(prev, res_ty), (x, arg_ty), (b, CPL.tbool)] $ do
        -- f :: arg_ty -> res_ty
        QMaxFunArg call_ufun <- lift $ view $ to mk_ucall
        QMaxFunArg aux_tys <- lift $ view $ to uproc_aux_types

        out <- Compiler.allocLocalWithPrefix "out" res_ty
        aux <- mapM Compiler.allocLocal aux_tys
        Compiler.addUStmt $ call_ufun $ map CQPL.Arg (x : out : aux)

        Compiler.addUStmt $
          CQPL.UnitaryS [CQPL.Arg prev, CQPL.Arg out, CQPL.Arg b] $
            CQPL.RevEmbedU ["a", "b"] $
              CPL.BinOpE CPL.LtOp (CPL.VarE "a") (CPL.VarE "b")

      Compiler.addProc cmp
      return cmp

    grover_k <- do
      meta_k <- Compiler.newIdent "k"
      x <- Compiler.newIdent "x"
      prev <- Compiler.newIdent "y"
      b <- Compiler.newIdent "b"

      uproc <- Compiler.buildUProc "Grover" [meta_k] [(prev, res_ty), (x, arg_ty), (b, CPL.tbool)] $ do
        let aux_tys = CQPL.proc_param_types cmp & drop 3
        aux_vars <- mapM Compiler.allocLocal aux_tys
        Compiler.addUStmt $
          groverK
            (CPL.MetaName meta_k)
            (x, arg_ty)
            b
            (\_ _ -> CQPL.UCallS (CQPL.proc_name cmp) False (map CQPL.Arg ([prev, x, b] ++ aux_vars)))

      Compiler.addProc uproc
      return uproc

    let n = CPL.domainSize arg_ty
    let max_queries = ceiling (_WQMax n eps) :: size
    let fuel_ty = CPL.Fin max_queries

    -- Build: qsearch with bounded fuel
    qsearch <- do
      (fuel, y, x) <- forOf each ("fuel", "y", "x") Compiler.newIdent
      p <- Compiler.buildProc "QSearch_infty" [] [(fuel, fuel_ty), (y, res_ty), (x, arg_ty)] $ do
        -- compute the limits for sampling `j` in each iteration.
        sampling_ranges <- do
          let lambda = 6 / 5
          let sqrt_n = sqrt (fromIntegral n)

          let go :: size -> [size] -> [size]
              go _ [] = []
              go lim (q : _) | q > lim = []
              go lim (q : qs) = q : go (lim - q) qs

          let nxt :: Float -> Float; nxt m = min (lambda * m) sqrt_n
          let js_f :: [Float]; js_f = lambda : map nxt js_f
          let js :: [size]; js = map floor js_f

          return $ go max_queries js

        -- body
        j_lim <- Compiler.allocLocalWithPrefix "j_lim" fuel_ty
        j <- Compiler.allocLocalWithPrefix "j" fuel_ty
        not_done <- Compiler.allocLocalWithPrefix "not_done" CPL.tbool
        b <- Compiler.allocLocalWithPrefix "b" CPL.tbool
        Compiler.addStmt $ CQPL.AssignS [not_done] (CPL.ConstE (CPL.FinV 1) CPL.tbool)

        Compiler.withStmt (CQPL.ForInArray j_lim fuel_ty [CPL.ConstE (CPL.FinV v_j) fuel_ty | v_j <- sampling_ranges]) $ do
          Compiler.addStmt $ CQPL.RandomDynS j j_lim
          Compiler.addStmt $ CQPL.AssignS [not_done] (CPL.VarE not_done CPL..&&. (CPL.VarE j CPL..<=. CPL.VarE fuel))
          Compiler.withStmt (CQPL.ifThenS not_done) $ do
            Compiler.addStmt $ CQPL.AssignS [fuel] (CPL.BinOpE CPL.SubOp (CPL.VarE fuel) (CPL.VarE j))
            Compiler.addStmt $
              CQPL.CallS
                { CQPL.fun = CQPL.UProcAndMeas (CQPL.proc_name grover_k)
                , CQPL.meta_params = [Right j]
                , CQPL.args = map CQPL.Arg [y, x, b]
                }
            Compiler.addStmt $ CQPL.AssignS [not_done] (CPL.VarE not_done CPL..&&. CPL.notE (CPL.VarE b))

      Compiler.addProc p
      return p

    -- Build the main algorithm
    Compiler.buildProc "QMax" [] [(res_var, res_ty), (argmax_var, arg_ty)] $ do
      fuel <- Compiler.allocLocalWithPrefix "fuel" fuel_ty
      Compiler.addStmt $ CQPL.AssignS [fuel] (CPL.ConstE (CPL.FinV $ max_queries - 1) fuel_ty)

      -- choose x \in arg_ty uniformly at random
      let x = argmax_var
      Compiler.addStmt $ CQPL.RandomS [x] (CPL.UniformE arg_ty)

      -- set y <- f(x);
      let y = res_var
      QMaxFunArg call_fun <- lift $ view $ to mk_call
      Compiler.addStmt $ call_fun [CQPL.Arg x, CQPL.Arg y]

      Compiler.withStmt (CQPL.RepeatS (CPL.MetaSize max_queries)) $ do
        Compiler.addStmt $ CQPL.CallS (CQPL.FunctionCall (CQPL.proc_name qsearch)) [] [CQPL.Arg fuel, CQPL.Arg y, CQPL.Arg x]
