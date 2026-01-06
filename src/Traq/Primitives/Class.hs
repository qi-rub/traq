{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Traq.Primitives.Class (
  -- * Primitives
  Primitive (..),
  PrimFnShape,
  ValidPrimShape (..),
  reshapeUnsafe,

  -- ** Typeclasses
  SerializePrim (..),
  TypeCheckPrim (..),
  EvalPrim (..),

  -- ** Unitary Compilation and Cost
  UnitaryCostPrim (..),
  UnitaryQueries (..),
  strongQueries,
  weakQueries,
  zeroQ,

  -- ** Classical-Quantum Compilation and Cost
  QuantumHavocCostPrim (..),
  QuantumExpCostPrim (..),

  -- ** Partial Functions
  PartialFun (..),
  placeArgs,
) where

import Control.Applicative (Alternative ((<|>)), many)
import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Control.Monad.Extra (concatMapM)
import Control.Monad.Reader (runReaderT)
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec (try)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (..), TokenParser)
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import qualified Traq.Data.Symbolic as Sym

import qualified Traq.Analysis as A
import qualified Traq.Analysis.CostModel.Class as C
import Traq.Prelude
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

{- | A partially applied function
Syntax: @f(a_1, ..., a_n)@ where each @a_i@ is either an identifier, or a blank @_@
-}
data PartialFun = PartialFun
  { pfun_name :: Ident
  , pfun_args :: [Maybe Ident]
  }
  deriving (Eq, Show)

instance PP.ToCodeString PartialFun where
  build PartialFun{pfun_name, pfun_args} = do
    let args = PP.commaList $ map (fromMaybe "_") pfun_args
    PP.putWord $ printf "%s(%s)" pfun_name args

instance P.Parseable PartialFun where
  parseE TokenParser{..} = do
    pfun_name <- identifier
    pfun_args <- parens $ commaSep ((Nothing <$ symbol "_") <|> (Just <$> identifier))
    return PartialFun{..}

instance P.RenameVars PartialFun where
  renameVars pref f@PartialFun{pfun_args} = f{pfun_args = map (fmap (P.addOnePrefix pref)) pfun_args}

{- | Place a list of concrete values inside a list of incomplete values.
For example, @placeArgs [Just 0, Nothing, Just 2, Nothing] [1, 3] = [0,1,2,3]@
-}
placeArgs :: [Maybe a] -> [a] -> [a]
placeArgs [] [] = []
placeArgs (Just x : xs) as = x : placeArgs xs as
placeArgs (Nothing : xs) (a : as) = a : placeArgs xs as
placeArgs mxs ys = error $ printf "invalid use of placeArgs(%d, %d)" (length mxs) (length ys)

{- | A generic second-order primitive.
It accepts a sequence of partially applied functions.

Authors are provided with simpler type classes to implement the features:
typing, semantics and query costs.
-}
data Primitive prim = Primitive [PartialFun] prim
  deriving (Eq, Show)

type instance SizeType (Primitive p) = SizeType p
type instance PrecType (Primitive p) = PrecType p

instance P.RenameVars (Primitive p) where
  renameVars pref (Primitive par_funs p) = Primitive (map (P.renameVars pref) par_funs) p

{- | The shape of the function arguments that primitive @prim@ expects.
The type @PrimFnShape prim a@ should be a subtype of @[a]@, for every @a@.
This type is useful to specify record constructor names for readability.
-}
type family PrimFnShape prim :: Type -> Type

-- | Conversion between shaped information and a list.
class ValidPrimShape shape where
  listToShape :: [a] -> Either String (shape a)
  shapeToList :: shape a -> [a]

reshapeUnsafe :: (ValidPrimShape shape, ValidPrimShape shape') => shape a -> shape' a
reshapeUnsafe = either (error "please typecheck first") id . listToShape . shapeToList

instance ValidPrimShape [] where
  listToShape = Right
  shapeToList = id

-- ================================================================================
-- Basic Instances
-- ================================================================================

instance (P.MapSize prim) => P.MapSize (Primitive prim) where
  type MappedSize (Primitive prim) size' = Primitive (P.MappedSize prim size')
  mapSize f (Primitive par_funs prim) = Primitive par_funs (P.mapSize f prim)

instance P.HasFreeVars (Primitive prim) where
  freeVarsList (Primitive par_funs _) = concatMap (catMaybes . pfun_args) par_funs

-- ================================================================================
-- Specialized Typeclasses for Primitives.
-- ================================================================================

-- --------------------------------------------------------------------------------
-- Printing and Parsing
-- --------------------------------------------------------------------------------

{- | Simple class to print/parse second-order primitives.
Syntax: `@prim<args, ...>[fns, ...];`
-}
class SerializePrim prim where
  -- | all names (variants) used by the primitive
  primNames :: [Ident]

  -- | name for a specific constructor
  primNameOf :: prim -> Ident
  primNameOf _ = case (primNames @prim) of
    [s] -> s
    _ -> error "primitive has multiple names, please override primNameOf"

  -- | parse the primitive info given its name
  parsePrimParams :: (SizeType prim ~ Sym.Sym SizeT) => TokenParser () -> Ident -> Parser prim

  -- | print the primitives parameters.
  printPrimParams :: prim -> [String]

-- Pretty Printing
instance (SerializePrim prim) => PP.ToCodeString (Primitive prim) where
  build (Primitive par_funs prim) = do
    fns <- concatMapM PP.fromBuild par_funs
    PP.putWord $ printf "@%s<%s>[%s]" (primNameOf prim) (PP.commaList $ printPrimParams prim) fns

-- Parsing
instance (SerializePrim prim, SizeType prim ~ Sym.Sym SizeT) => P.Parseable (Primitive prim) where
  parseE tp@TokenParser{..} = parseFindXorPeriod
   where
    parseFindXorPeriod = do
      ('@' : name) <- foldr1 (<|>) $ map (\s -> try $ symbol $ "@" ++ s) $ primNames @prim
      prim <- angles $ parsePrimParams tp name
      par_funs <- brackets $ many $ P.parseE tp
      return $ Primitive par_funs prim

-- --------------------------------------------------------------------------------
-- Typing
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
    fn_tys <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      P.FunDef{P.param_types, P.ret_types} <-
        view (Ctx.at pfun_name)
          >>= maybeWithError (printf "cannot find function argument `%s`" pfun_name)

      when (length pfun_args /= length param_types) $
        throwError "Invalid number of function arguments"

      prim_arg_tys <- forM (zip pfun_args param_types) $ \(mvar, ty) -> do
        case mvar of
          Just var -> do
            var_ty <- Ctx.lookup var
            when (var_ty /= ty) $ throwError "invalid arg type to bind"
            return Nothing
          Nothing -> return $ Just ty

      return $ P.FnType (catMaybes prim_arg_tys) ret_types

    shaped_fn_tys <- liftEither $ listToShape fn_tys

    inferRetTypesPrim prim shaped_fn_tys

-- --------------------------------------------------------------------------------
-- Evaluation
-- --------------------------------------------------------------------------------

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
    fns_eval <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' = P.evalFun (placeArgs vs vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}
      return eval_fn

    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    evalPrim prim shaped_fns_eval

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

    let tot_eps_fns = A.failProb $ sum $ zipWith calc_tot_err query_costs eps_fn
    return $ eps + tot_eps_fns
   where
    conv :: prec -> prec
    conv eps' = sqrt (2 * eps')

    calc_tot_err :: prec -> A.FailProb prec -> prec
    calc_tot_err n eps' = n * conv (A.getFailProb eps')

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

-- --------------------------------------------------------------------------------
-- Quantum Compiler: Costs, Error.
-- --------------------------------------------------------------------------------

{- | Worst-case Quantum query and operation costs of a primitive.
Represents one level of the call graph.
The quantum compiler of the primitive can use both the quantum and unitary compilation of its function arguments.
-}
class
  ( size ~ SizeType prim
  , prec ~ PrecType prim
  , A.SizeToPrec size prec
  , TypeCheckPrim prim size
  ) =>
  QuantumHavocCostPrim prim size prec
    | prim -> size prec
  where
  -- | Bound on number of queries made to each function's quantum compilation.
  quantumQueryCostsQuantum :: prim -> A.FailProb prec -> PrimFnShape prim prec

  -- | Bound on number of queries made to each function's unitary compilation.
  quantumQueryCostsUnitary :: prim -> A.FailProb prec -> PrimFnShape prim (UnitaryQueries prec)

  -- | Cost of all additional operations. Defaults to zero.
  quantumExprCosts :: (C.CostModel cost, precT ~ PrecType cost) => prim -> A.FailProb prec -> cost
  quantumExprCosts _ _ = Alg.zero

instance
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , A.ErrorReqs size prec
  ) =>
  A.TVErrorQ (A.AnnFailProb (Primitive prim)) size prec
  where
  tvErrorQ (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps
    let query_costs_u = map totalWeakUnitaryQueries . shapeToList $ quantumQueryCostsUnitary prim eps

    eps_fns <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      eps_q <- A.tvErrorQ fn
      eps_u <- A.traceNormErrorU fn
      return (eps_q, eps_u)
    let (eps_fns_q, eps_fns_u) = unzip eps_fns

    let tot_eps_u = A.failProb $ sum $ zipWith calc_tot_err_u query_costs_u eps_fns_u
    let tot_eps_q = A.failProb $ sum $ zipWith calc_tot_err_q query_costs_q eps_fns_q
    return $ eps + tot_eps_q + tot_eps_u
   where
    conv :: prec -> prec
    conv eps' = sqrt (2 * eps')

    calc_tot_err_u :: prec -> A.FailProb prec -> prec
    calc_tot_err_u n eps' = n * conv (A.getFailProb eps')

    calc_tot_err_q :: prec -> A.FailProb prec -> prec
    calc_tot_err_q n eps' = n * A.getFailProb eps'

instance
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , A.CostReqs size prec
  ) =>
  A.CostQ (A.AnnFailProb (Primitive prim)) size prec
  where
  costQ (A.AnnFailProb eps (Primitive par_funs prim)) = do
    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps
    let query_costs_u = map totalWeakUnitaryQueries . shapeToList $ quantumQueryCostsUnitary prim eps

    fn_costs_uq <- forM par_funs $ \PartialFun{pfun_name} -> do
      fn <- view $ P._funCtx . Ctx.at pfun_name . non' (error "invalid function")
      cost_u <- A.costU $ P.NamedFunDef pfun_name fn
      cost_q <- A.costQ $ P.NamedFunDef pfun_name fn
      return (cost_u, cost_q)
    let (fn_costs_u, fn_costs_q) = unzip fn_costs_uq

    let tot_cost_q = Alg.sum $ zipWith (Alg..*) query_costs_q fn_costs_q
    let tot_cost_u = Alg.sum $ zipWith (Alg..*) query_costs_u fn_costs_u
    -- TODO expression cost
    return $ Alg.sum [tot_cost_q, tot_cost_u]

instance
  ( QuantumHavocCostPrim prim size prec
  , UnitaryCostPrim prim size prec
  , P.TypingReqs size
  , Floating prec
  , Ord prec
  ) =>
  A.QuantumHavocCost (Primitive prim) size prec
  where
  quantumHavocCost eps (Primitive par_funs prim) = do
    -- split the overall precision in half
    let eps_alg = A.divideError eps 2

    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let query_costs_u = map (\q -> strong q + weak q) . shapeToList $ quantumQueryCostsUnitary prim eps_alg

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
  , A.SizeToPrec size prec
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
    shape (UnitaryQueries prec)

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
  ( UnitaryCostPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , QuantumExpCostPrim prim size prec
  , EvalPrim prim size prec
  , A.CostReqs size prec
  , P.EvalReqs size prec
  ) =>
  A.ExpCostQ (A.AnnFailProb (Primitive prim)) size prec
  where
  expCostQ (A.AnnFailProb eps (Primitive par_funs prim)) sigma = do
    -- Extract the semantics of each function argument.
    eval_env <- view P._evaluationEnv

    fns_with_args_and_eval <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' =
            P.evalFun (placeArgs vs vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}
              & (runReaderT ?? eval_env)

      return ((P.NamedFunDef pfun_name fn, vs), eval_fn)

    let (fns_with_args, fns_eval) = unzip fns_with_args_and_eval
    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    -- expected queries
    let exp_query_costs_q = shapeToList $ quantumExpQueryCostsQuantum prim eps shaped_fns_eval
    let exp_query_costs_u = map totalWeakUnitaryQueries . shapeToList $ quantumExpQueryCostsUnitary prim eps shaped_fns_eval

    fn_costs <- forM (zip3 fns_with_args exp_query_costs_q exp_query_costs_u) $
      \((fn, pref_args), exp_queries_q, exp_queries_u) -> do
        -- queries to quantum f
        q_costs <- forM exp_queries_q $ \(vs, eq) -> do
          let sigma_fn =
                Ctx.fromList $
                  zip
                    [show i | i <- [0 :: Int ..]]
                    (placeArgs pref_args vs)
          q_f <- A.expCostQ fn sigma_fn
          return $ eq Alg..* q_f

        -- queries to unitary f
        cost_f_u <- A.costU fn
        let u_cost = exp_queries_u Alg..* cost_f_u

        return $ Alg.sum q_costs Alg.+ u_cost

    -- all other non-query operations
    let extra_costs = quantumExpExprCosts prim eps shaped_fns_eval

    return $ Alg.sum fn_costs Alg.+ extra_costs

instance
  ( QuantumExpCostPrim prim size prec
  , EvalPrim prim size prec
  , QuantumHavocCostPrim prim size prec
  , UnitaryCostPrim prim size prec
  , P.EvalReqs size prec
  , Floating prec
  , Ord prec
  ) =>
  A.QuantumExpCost (Primitive prim) size prec
  where
  quantumExpCost eps (Primitive par_funs prim) sigma = do
    -- Extract the semantics of each function argument.
    eval_env <- view P._evaluationEnv

    fns_eval_and_args <- forM par_funs $ \PartialFun{pfun_name, pfun_args} -> do
      fn <-
        view $
          P._funCtx
            . Ctx.at pfun_name
            . to (fromMaybe (error "unable to find predicate, please typecheck first!"))

      let vs = flip map pfun_args $ \case
            Just x -> Just $ sigma ^. Ctx.at x . non (error "ill-formed program")
            Nothing -> Nothing

      let eval_fn vs' =
            P.evalFun (placeArgs vs vs') P.NamedFunDef{P.fun_name = pfun_name, P.fun_def = fn}
              & (runReaderT ?? eval_env)

      return (eval_fn, vs)

    let (fns_eval, fns_args) = unzip fns_eval_and_args
    let shaped_fns_eval = either (error "please typecheck first") id $ listToShape fns_eval

    -- Compute the expected cost
    -- split the overall precision in half
    let eps_alg = A.divideError eps 2

    -- worst case queries
    let query_costs_q = shapeToList $ quantumQueryCostsQuantum prim eps_alg
    let query_costs_u = map (\q -> strong q + weak q) . shapeToList $ quantumQueryCostsUnitary prim eps_alg

    -- expected queries
    let exp_query_costs_q = shapeToList $ quantumExpQueryCostsQuantum prim eps_alg shaped_fns_eval
    let exp_query_costs_u = map (\q -> strong q + weak q) . shapeToList $ quantumExpQueryCostsUnitary prim eps_alg shaped_fns_eval

    -- split the other half into equal parts per function
    let eps_fns = A.divideError (eps - eps_alg) (A.sizeToPrec $ length par_funs)

    fn_costs <- forM (zip3 (zip par_funs fns_args) (zip query_costs_q exp_query_costs_q) (zip query_costs_u exp_query_costs_u)) $
      \((PartialFun{pfun_name}, pref_args), (n_queries_q, exp_queries_q), (n_queries_u, exp_queries_u)) -> do
        -- divide by number of queries to get cost per call
        let eps_fn = A.divideError eps_fns (n_queries_u + n_queries_q)

        -- queries to quantum f
        q_costs <- forM exp_queries_q $ \(vs, eq) -> do
          q_f <- A.quantumQueryCostF eps_fn (placeArgs pref_args vs) pfun_name
          return $ eq Alg..* q_f

        -- queries to unitary f
        cost_f_u <- A.unitaryQueryCostF (A.requiredFailProbToNormError eps_fn) pfun_name
        let u_cost = 2 * exp_queries_u Alg..* cost_f_u

        return $ Alg.sum q_costs Alg.+ u_cost

    -- all other non-query operations
    let extra_costs = quantumExpExprCosts prim eps_alg shaped_fns_eval

    return $ Alg.sum fn_costs Alg.+ extra_costs
