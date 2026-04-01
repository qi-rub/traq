{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.Primitives.Search.DetSearch (
  DetSearch (..),
) where

import Control.Monad.Except (throwError)
import GHC.Generics (Generic)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import qualified Traq.Data.Probability as Prob
import Traq.Data.Subtyping

import qualified Traq.Analysis as A
import qualified Traq.CPL as CPL
import qualified Traq.Compiler as Compiler
import Traq.Prelude
import Traq.Primitives.Class
import Traq.Primitives.Search.Prelude
import qualified Traq.QPL as QPL

-- ================================================================================
-- Primitive Implementation
-- ================================================================================

-- | Primitive implementing brute-force classical search.
newtype DetSearch size prec = DetSearch (PrimSearch size prec)
  deriving (Eq, Show, Read, Generic)

type instance SizeType (DetSearch size prec) = size
type instance PrecType (DetSearch size prec) = prec

type instance PrimFnShape (DetSearch size prec) = BooleanPredicate

instance CPL.MapSize (DetSearch size prec) where
  type MappedSize (DetSearch size prec) size' = DetSearch size' prec
  mapSize f (DetSearch p) = DetSearch (CPL.mapSize f p)

instance CPL.MapPrec (DetSearch size prec) where
  type MappedPrec (DetSearch size prec) prec' = DetSearch size prec'
  mapPrec f (DetSearch p) = DetSearch (CPL.mapPrec f p)

instance PrimSearch size prec :<: DetSearch size prec

instance (Show size) => SerializePrim (DetSearch size prec) where
  primNames = ["any"]
  parsePrimParams tp s = DetSearch <$> parsePrimParams tp s
  printPrimParams (DetSearch prim) = printPrimParams prim

instance (CPL.TypingReqs size) => TypeCheckPrim (DetSearch size prec) size where
  inferRetTypesPrim (DetSearch prim) = inferRetTypesPrim prim

instance EvalPrim (DetSearch size prec) size prec where
  evalPrim (DetSearch prim) = evalPrim prim

-- ================================================================================
-- Abstract Costs
-- ================================================================================

instance (CPL.TypingReqs size, Integral size, Num prec) => UnitaryCostPrim (DetSearch size prec) size prec where
  unitaryQueryCosts (DetSearch PrimSearch{search_ty}) _ = BooleanPredicate (weakQueries $ fromIntegral _N)
   where
    _N = CPL.domainSize search_ty

  unitaryExprCosts _ _ = Alg.zero

instance (CPL.TypingReqs size, Integral size, Num prec, A.SizeToPrec size prec) => QuantumHavocCostPrim (DetSearch size prec) size prec where
  -- only classical queries
  quantumQueryCostsQuantum (DetSearch PrimSearch{search_ty}) _ = BooleanPredicate (fromIntegral _N)
   where
    _N = CPL.domainSize search_ty

  -- no unitary
  quantumQueryCostsUnitary _ _ = BooleanPredicate $ weakQueries 0

  quantumExprCosts = Alg.zero

instance
  (size ~ SizeT, Floating prec, Alg.Monoidal prec, Alg.Semiring prec) =>
  QuantumExpCostPrim (DetSearch size prec) size prec
  where
  quantumExpQueryCostsQuantum (DetSearch PrimSearch{search_ty}) _ (BooleanPredicate eval_pred) =
    BooleanPredicate [([v], 1) | v <- queried_vals]
   where
    results =
      CPL.domain search_ty <&> \v ->
        case Prob.toDeterministicValue $ eval_pred [v] of
          Just [b] -> (CPL.valueToBool b, v)
          _ -> error "predicate is not determinisic"

    -- query all values till the first solution.
    (non_sols, sol_and_rest) = break fst results & (each %~ map snd)
    queried_vals = non_sols ++ take 1 sol_and_rest

  quantumExpQueryCostsUnitary _ _ _ = BooleanPredicate $ weakQueries 0

  quantumExpExprCosts = Alg.zero

-- ================================================================================
-- Compilation
-- ================================================================================

instance (Integral size) => UnitaryCompilePrim (DetSearch size prec) size prec where
  compileUPrim (DetSearch PrimSearch{search_ty, search_kind}) _ = do
    (BooleanPredicate call_upred) <- view $ to mk_ucall
    (BooleanPredicate pred_aux_tys) <- view $ to uproc_aux_types

    let _N = CPL.domainSize search_ty

    ok <- Compiler.newIdent "ok"
    x_out <- Compiler.newIdent "x"

    let rets = case search_kind of
          SearchK -> [(ok, CPL.tbool), (x_out, search_ty)]
          _ -> [(ok, CPL.tbool)]

    Compiler.buildUProc "UDetSearch" [] rets $ do
      x <- case search_kind of
        SearchK -> return x_out
        _ -> Compiler.allocLocalWithPrefix "x" search_ty
      tmp_flag <- Compiler.allocLocalWithPrefix "tmp_flag" $ CPL.Arr _N CPL.tbool
      tmp_res <- Compiler.allocLocalWithPrefix "tmp_x" $ CPL.Arr _N search_ty
      aux <- mapM (Compiler.allocLocal . CPL.Arr _N) pred_aux_tys

      i <- Compiler.newIdent "i"
      Compiler.withUStmt (QPL.UForInDomainS i (CPL.Fin _N) False) $ do
        let tmp_flag_ix = (`QPL.ArrElemArg` CPL.MetaName i) $ QPL.Arg tmp_flag
        let tmp_res_ix = (`QPL.ArrElemArg` CPL.MetaName i) $ QPL.Arg tmp_res
        let aux_ix = map ((`QPL.ArrElemArg` CPL.MetaName i) . QPL.Arg) aux

        Compiler.addUStmt $ QPL.UnitaryS [tmp_res_ix] (QPL.RevEmbedU [] (CPL.ParamE i))
        Compiler.addUStmt $ call_upred (tmp_res_ix : tmp_flag_ix : aux_ix)

      Compiler.addUStmt $ QPL.UnitaryS (map QPL.Arg [tmp_flag, tmp_res, ok, x]) (QPL.NamedGateU "SelectOnTrue")

instance (Integral size) => QuantumCompilePrim (DetSearch size prec) size prec where
  compileQPrim (DetSearch PrimSearch{search_kind, search_ty}) _ = do
    ret_tys <- view $ to prim_ret_types
    (flag_ty, sample_ty) <- case ret_tys of
      [b, t] -> return (b, t)
      [b] -> return (b, search_ty)
      _ -> throwError "typecheck failed"

    flag <- Compiler.newIdent "ok"
    x_out <- Compiler.newIdent "x"

    (BooleanPredicate call_pred) <- view $ to mk_call

    let _N = CPL.domainSize search_ty

    let ret_params = case search_kind of
          SearchK -> [(flag, flag_ty), (x_out, sample_ty)]
          _ -> [(flag, flag_ty)]

    Compiler.buildProc "DetSearch" [] ret_params $ do
      sample <- case search_kind of
        SearchK -> return x_out
        _ -> Compiler.allocLocalWithPrefix "x" search_ty
      i <- Compiler.newIdent "i"
      Compiler.withStmt (QPL.ForInRangeS i (CPL.MetaSize _N)) $ do
        Compiler.addStmt $
          QPL.IfThenElseS
            { cond = flag
            , s_true = QPL.SkipS
            , s_false =
                QPL.SeqS
                  [ QPL.AssignS [sample] (CPL.ParamE i)
                  , call_pred [QPL.Arg flag, QPL.Arg sample]
                  ]
            }
