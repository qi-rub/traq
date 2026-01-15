{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class.UnitaryCost (
  UnitaryCostPrim (..),
  UnitaryQueries (..),
  strongQueries,
  weakQueries,
  zeroQ,
  totalWeakUnitaryQueries,
) where

import Control.Monad (forM, forM_, void, when)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis as A
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Prelude
import Traq.Primitives.Class.Eval
import Traq.Primitives.Class.Prelude
import Traq.Primitives.Class.TypeCheck
import qualified Traq.ProtoLang as P

-- --------------------------------------------------------------------------------
-- Unitary Compiler: Costs, Error.
-- --------------------------------------------------------------------------------

data UnitaryQueries prec = UnitaryQueries {strong, weak :: prec}
  deriving (Eq, Read, Show)

strongQueries, weakQueries :: (Num prec) => prec -> UnitaryQueries prec
strongQueries q = UnitaryQueries{strong = q, weak = 0}
weakQueries q = UnitaryQueries{strong = 0, weak = q}

zeroQ :: (Num prec) => UnitaryQueries prec
zeroQ = UnitaryQueries{strong = 0, weak = 0}

-- | Total number of queries to a "weak" (i.e. with entangled aux) implementation of the sub-function.
totalWeakUnitaryQueries :: (Num prec) => UnitaryQueries prec -> prec
totalWeakUnitaryQueries UnitaryQueries{strong, weak} = 2 * strong + weak

{- | Unitary query and operation costs of a primitive.
Represents one level of the call graph.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , TypeCheckPrim prim size
  ) =>
  UnitaryCostPrim prim size prec
    | prim -> size prec
  where
  -- | Bound on number of queries made to each function.
  unitaryQueryCosts :: prim -> A.FailProb prec -> PrimFnShape prim (UnitaryQueries prec)

  -- | Cost of all additional operations. Defaults to zero.
  unitaryExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => prim -> A.FailProb prec -> cost
  unitaryExprCosts _ _ = Alg.zero

instance
  ( UnitaryCostPrim prim size prec
  , A.ErrorReqs size prec
  ) =>
  A.TraceNormErrorU (A.AnnFailProb (Primitive prim)) size prec
  where
  traceNormErrorU (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs = map totalWeakUnitaryQueries . shapeToList $ unitaryQueryCosts prim eps
    eps_fn <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      A.traceNormErrorU fn

    let tot_eps_fns = sum $ zipWith A.unitarySubroutineTVErrorTotal query_costs eps_fn
    return $ eps + tot_eps_fns

instance
  ( UnitaryCostPrim prim size prec
  , A.CostReqs size prec
  ) =>
  A.CostU (A.AnnFailProb (Primitive prim)) size prec
  where
  costU (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs = map totalWeakUnitaryQueries . shapeToList $ unitaryQueryCosts prim eps

    fn_costs <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      A.costU $ P.NamedFunDef pfun_name fn

    -- TODO expression cost
    return $ Alg.sum $ zipWith (Alg..*) query_costs fn_costs

instance
  ( UnitaryCostPrim prim size prec
  , P.TypingReqs size
  , Floating prec
  , A.SizeToPrec size prec
  ) =>
  A.UnitaryCost (Primitive prim) size prec
  where
  unitaryCost delta (Primitive par_funs prim) = do
    -- split the overall precision in half
    let delta_alg = A.divideError delta 2
    let eps_alg = A.requiredNormErrorToFailProb delta_alg

    let query_costs = map (\q -> strong q + weak q) . shapeToList $ unitaryQueryCosts prim eps_alg

    -- split the other half into equal parts per function
    let delta_fns = A.divideError (delta - delta_alg) (A.sizeToPrec $ length par_funs)

    fn_costs <- forM (zip par_funs query_costs) $ \(PartialFun{pfun_name}, n_queries) -> do
      -- divide by number of queries to get cost per call
      let delta_fn = A.divideError delta_fns n_queries
      let delta_fn_dirty = A.divideError delta_fn 2

      -- cost per call to f, with the same precision.
      cost_f <- A.unitaryQueryCostF delta_fn_dirty pfun_name

      return $ (2 * n_queries) Alg..* cost_f

    -- all other non-query operations
    let extra_costs = unitaryExprCosts prim eps_alg

    -- 2x for compute-uncompute
    return $ (2 :: prec) Alg..* (Alg.sum fn_costs Alg.+ extra_costs)
