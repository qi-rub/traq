{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class (
  Primitive (..),
  PartialFun (..),
  PrimFnShape,
  ValidPrimShape (..),

  -- * Typeclasses
  TypeCheckPrim (..),
  EvalPrim (..),
  UnitaryCostPrim (..),
  QuantumHavocCostPrim (..),
  QuantumExpCostPrim (..),
) where

import Control.Applicative (many)
import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (runReaderT)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Text.Parsec.Token (GenTokenParser (..))
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis as A
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

data PartialFun = PartialFun
  { pfun_name :: Ident
  , prefix_args :: [Ident]
  }
  deriving (Eq, Show)

instance PP.ToCodeString PartialFun where
  build PartialFun{pfun_name, prefix_args} = do
    PP.putWord $ printf "%s(%s)" pfun_name (PP.commaList prefix_args)

{- | A generic second-order primitive.
It accepts a sequence of partially applied functions.

Authors are provided with simpler type classes to implement the features:
typing, semantics and query costs.
-}
data Primitive prim = Primitive [PartialFun] prim
  deriving (Eq, Show)

type instance SizeType (Primitive p) = SizeType p
type instance PrecType (Primitive p) = PrecType p

{- | The shape of the function arguments that primitive @prim@ expects.
The type @PrimFnShape prim a@ should be a subtype of @[a]@, for every @a@.
This type is useful to specify record constructor names for readability.
-}
type family PrimFnShape prim :: Type -> Type

-- | Conversion between shaped information and a list.
class ValidPrimShape shape where
  listToShape :: [a] -> Either String (shape a)
  shapeToList :: shape a -> [a]

-- ================================================================================
-- Basic Instances
-- ================================================================================

instance (P.MapSize prim) => P.MapSize (Primitive prim) where
  type MappedSize (Primitive prim) size' = Primitive (P.MappedSize prim size')
  mapSize f (Primitive par_funs prim) = Primitive par_funs (P.mapSize f prim)

-- Pretty Printing
instance (PP.ToCodeString prim) => PP.ToCodeString (Primitive prim) where
  build (Primitive par_funs prim) = do
    prim_s <- PP.fromBuild prim
    fns <- concatMapM PP.fromBuild par_funs
    PP.putWord $ printf "%s[%s]" prim_s fns

-- Parsing
instance (P.Parseable prim) => P.Parseable (Primitive prim) where
  parseE tp@TokenParser{..} = parseFindXorPeriod
   where
    parseFindXorPeriod = do
      prim <- P.parseE tp
      par_funs <- brackets $ many $ do
        pfun_name <- identifier
        prefix_args <- parens $ commaSep identifier
        return PartialFun{..}
      return $ Primitive par_funs prim

instance P.HasFreeVars (Primitive prim) where
  freeVarsList (Primitive par_funs _) = concatMap prefix_args par_funs

-- ================================================================================
-- Specialized Typeclasses for Primitives.
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Typing and Evaluation
-- --------------------------------------------------------------------------------

{- | Type check a primitive given the types of its function arguments, and infer the return types.
The typechecker internally checks that the bound arguments are correct,
and only gives the user the final type of the partial function.
-}
class
  ( size ~ SizeType prim
  , ValidPrimShape (PrimFnShape prim)
  ) =>
  TypeCheckPrim prim size
    | prim -> size
  where
  inferRetTypesPrim ::
    forall ext' shape m.
    ( m ~ P.TypeChecker ext'
    , size ~ SizeType ext'
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape (P.FnType size) ->
    m [P.VarType size]

instance (TypeCheckPrim prim size, P.TypingReqs size) => P.TypeInferrable (Primitive prim) size where
  inferTypes (Primitive par_funs prim) = do
    fn_tys <- forM par_funs $ \PartialFun{pfun_name, prefix_args} -> do
      P.FunDef{P.param_types, P.ret_types} <-
        view (Ctx.at pfun_name)
          >>= maybeWithError (printf "cannot find function argument `%s`" pfun_name)

      arg_tys <- mapM Ctx.lookup prefix_args
      let n_args = length arg_tys

      when (take n_args param_types /= arg_tys) $
        throwError "Invalid arguments to bind to function"

      return $ P.FnType (drop n_args param_types) ret_types

    shaped_fn_tys <- liftEither $ listToShape fn_tys

    inferRetTypesPrim prim shaped_fn_tys

{- | Evaluate a primitive given the semantics of each function argument.
For partial functions, the prefix of arguments are already bound.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , ValidPrimShape (PrimFnShape prim)
  ) =>
  EvalPrim prim size prec
    | prim -> size prec
  where
  evalPrim ::
    forall ext' shape m.
    ( P.Evaluatable ext' size prec
    , m ~ P.Evaluator ext'
    , SizeType ext' ~ size
    , PrecType ext' ~ prec
    , shape ~ PrimFnShape prim
    ) =>
    prim ->
    shape ([P.Value size] -> m [P.Value size]) ->
    m [P.Value size]

instance
  ( EvalPrim prim size prec
  , P.EvalReqs size prec
  ) =>
  P.Evaluatable (Primitive prim) size prec
  where
  eval (Primitive par_funs prim) sigma = do
    fns_eval <- forM par_funs $ \PartialFun{pfun_name, prefix_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = map (\x -> sigma ^. Ctx.at x . non (error "ill-formed program")) prefix_args
      return $ \vs' -> P.evalFun (vs ++ vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}

    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    evalPrim prim shaped_fns_eval

-- --------------------------------------------------------------------------------
-- Costs
-- --------------------------------------------------------------------------------

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
  unitaryQueryCosts :: prim -> A.FailProb prec -> PrimFnShape prim prec

  -- | Cost of all additional operations. Defaults to zero.
  unitaryExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => prim -> A.FailProb prec -> cost
  unitaryExprCosts _ _ = Alg.zero

instance
  ( UnitaryCostPrim prim size prec
  , P.TypingReqs size
  , Floating prec
  , P.SizeToPrec size prec
  ) =>
  P.UnitaryCost (Primitive prim) size prec
  where
  unitaryCost delta (Primitive par_funs prim) = do
    -- split the overall precision in half
    let delta_alg = A.divideError delta 2
    let eps_alg = A.requiredNormErrorToFailProb delta_alg

    let query_costs = shapeToList $ unitaryQueryCosts prim eps_alg

    -- split the other half into equal parts per function
    let delta_fns = A.divideError (delta - delta_alg) (A.sizeToPrec $ length par_funs)

    fn_costs <- forM (zip par_funs query_costs) $ \(PartialFun{pfun_name}, n_queries) -> do
      -- divide by number of queries to get cost per call
      let delta_fn = A.divideError delta_fns n_queries

      -- cost per call to f, with the same precision.
      cost_f <- A.unitaryQueryCostF delta_fn pfun_name

      return $ n_queries Alg..* cost_f

    -- all other non-query operations
    let extra_costs = unitaryExprCosts prim eps_alg

    -- 2x for compute-uncompute
    return $ (2 :: prec) Alg..* (Alg.sum fn_costs Alg.+ extra_costs)

{- | Worst-case Quantum query and operation costs of a primitive.
Represents one level of the call graph.
The quantum compiler of the primitive can use both the quantum and unitary compilation of its function arguments.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , P.SizeToPrec size prec
  , TypeCheckPrim prim size
  ) =>
  QuantumHavocCostPrim prim size prec
    | prim -> size prec
  where
  -- | Bound on number of queries made to each function's quantum compilation.
  quantumQueryCostsQuantum :: prim -> A.FailProb prec -> PrimFnShape prim prec

  -- | Bound on number of queries made to each function's unitary compilation.
  quantumQueryCostsUnitary :: prim -> A.FailProb prec -> PrimFnShape prim prec

  -- | Cost of all additional operations. Defaults to zero.
  quantumExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => prim -> A.FailProb prec -> cost
  quantumExprCosts _ _ = Alg.zero

instance
  ( QuantumHavocCostPrim prim size prec
  , UnitaryCostPrim prim size prec
  , P.TypingReqs size
  , Floating prec
  , Ord prec
  ) =>
  P.QuantumHavocCost (Primitive prim) size prec
  where
  quantumHavocCost eps (Primitive par_funs prim) = do
    -- split the overall precision in half
    let eps_alg = A.divideError eps 2

    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let query_costs_u = shapeToList $ quantumQueryCostsUnitary prim eps_alg

    -- split the other half into equal parts per function
    let eps_fns = A.divideError (eps - eps_alg) (A.sizeToPrec $ length par_funs)

    fn_costs <- forM (zip3 par_funs query_costs_q query_costs_u) $ \(PartialFun{pfun_name}, n_queries_q, n_queries_u) -> do
      -- divide by number of queries to get cost per call
      let eps_fn = A.divideError eps_fns (n_queries_u + n_queries_q)

      -- cost per call to f, with the same precision.
      cost_f_q <- A.quantumMaxQueryCostF eps_fn pfun_name
      cost_f_u <- A.unitaryQueryCostF (A.requiredFailProbToNormError eps_fn) pfun_name

      return $ (n_queries_q Alg..* cost_f_q) Alg.+ (2 * n_queries_u Alg..* cost_f_u)

    -- all other non-query operations
    let extra_costs = unitaryExprCosts prim eps_alg

    return $ Alg.sum fn_costs Alg.+ extra_costs

{- | Expected Quantum query and operation costs of a primitive.
Represents one level of the call graph.
The quantum compiler of the primitive can use both the quantum and unitary compilation of its function arguments.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , P.SizeToPrec size prec
  , TypeCheckPrim prim size
  ) =>
  QuantumExpCostPrim prim size prec
  where
  {- Bound on the expected number of queries made to each function's quantum compilation.
  This is a random variable over the inputs to each function.
  -}
  quantumExpQueryCostsQuantum ::
    forall shape m.
    ( shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape [([P.Value size], prec)]

  -- | Bound on the expected number of queries made to each function's unitary compilation.
  quantumExpQueryCostsUnitary ::
    forall shape m.
    (shape ~ PrimFnShape prim, m ~ P.EvaluationMonad prec) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    shape prec

  -- | Cost of all additional operations. Defaults to zero.
  quantumExpExprCosts ::
    forall shape cost m.
    ( C.CostModel cost
    , prec ~ PrecType cost
    , shape ~ PrimFnShape prim
    , m ~ P.EvaluationMonad prec
    ) =>
    prim ->
    A.FailProb prec ->
    shape ([P.Value size] -> m [P.Value size]) ->
    cost
  quantumExpExprCosts _ _ _ = Alg.zero

instance
  ( QuantumExpCostPrim prim size prec
  , EvalPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , UnitaryCostPrim prim size prec
  , P.EvalReqs size prec
  , Floating prec
  , Ord prec
  ) =>
  P.QuantumExpCost (Primitive prim) size prec
  where
  quantumExpCost eps (Primitive par_funs prim) sigma = do
    -- Extract the semantics of each function argument.
    eval_env <- view P._evaluationEnv

    fns_eval_and_args <- forM par_funs $ \PartialFun{pfun_name, prefix_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = map (\x -> sigma ^. Ctx.at x . non (error "ill-formed program")) prefix_args
      let fn_eval vs' =
            P.evalFun (vs ++ vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}
              & (runReaderT ?? eval_env)
      return (fn_eval, vs)

    let (fns_eval, fns_args) = unzip fns_eval_and_args
    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    -- Compute the expected cost
    -- split the overall precision in half
    let eps_alg = A.divideError eps 2

    -- worst case queries
    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let query_costs_u = shapeToList $ quantumQueryCostsUnitary prim eps_alg

    -- expected queries
    let exp_query_costs_q = shapeToList $ quantumExpQueryCostsQuantum prim eps_alg shaped_fns_eval
    let exp_query_costs_u = shapeToList $ quantumExpQueryCostsUnitary prim eps_alg shaped_fns_eval

    -- split the other half into equal parts per function
    let eps_fns = A.divideError (eps - eps_alg) (A.sizeToPrec $ length par_funs)

    fn_costs <- forM (zip3 (zip par_funs fns_args) (zip query_costs_q exp_query_costs_q) (zip query_costs_u exp_query_costs_u)) $
      \((PartialFun{pfun_name}, pref_args), (n_queries_q, exp_queries_q), (n_queries_u, exp_queries_u)) -> do
        -- divide by number of queries to get cost per call
        let eps_fn = A.divideError eps_fns (n_queries_u + n_queries_q)

        -- queries to quantum f
        q_costs <- forM exp_queries_q $ \(vs, eq) -> do
          q_f <- A.quantumQueryCostF eps_fn (pref_args ++ vs) pfun_name
          return $ eq Alg..* q_f

        -- queries to unitary f
        cost_f_u <- magnify P._unitaryCostEnv $ A.unitaryQueryCostF (A.requiredFailProbToNormError eps_fn) pfun_name
        let u_cost = 2 * exp_queries_u Alg..* cost_f_u

        return $ Alg.sum q_costs Alg.+ u_cost

    -- all other non-query operations
    let extra_costs = quantumExpExprCosts prim eps_alg shaped_fns_eval

    return $ Alg.sum fn_costs Alg.+ extra_costs
