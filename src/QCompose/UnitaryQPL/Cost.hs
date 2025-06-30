{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module QCompose.UnitaryQPL.Cost (
  stmtCost,
  procCost,
  programCost,

  -- * types
  ProcCtx,
  CostMap,
  CostCalculator,

  -- * Holes
  HoleCost (..),
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.UnitaryQPL.Syntax

-- | Cache the costs of each procedure
type CostMap costT = Map.Map Ident costT

-- | Environment: the list of procedures, and a mapping from holes to cost.
type CostEnv holeT sizeT costT = ProcCtx holeT sizeT costT

procCtx :: Lens' (CostEnv holeT sizeT costT) (ProcCtx holeT sizeT costT)
procCtx = id

-- | Monad to compute unitary cost.
type CostCalculator holeT sizeT costT = MyReaderStateT (CostEnv holeT sizeT costT) (CostMap costT) Maybe

-- | Compute the cost of a placeholder (hole) program.
class HoleCost holeT costT where
  holeCost :: forall sizeT. (Integral sizeT, Floating costT) => holeT -> CostCalculator holeT sizeT costT costT

instance HoleCost Void costT where
  holeCost = absurd

stmtCost ::
  ( Integral sizeT
  , Floating costT
  , HoleCost holeT costT
  , m ~ CostCalculator holeT sizeT costT
  ) =>
  Stmt holeT sizeT ->
  m costT
stmtCost SkipS = return 0
stmtCost (CommentS _) = return 0
stmtCost UnitaryS{} = return 0
stmtCost CallS{proc_id} = procCost proc_id
stmtCost (SeqS ss) = sum <$> mapM stmtCost ss
stmtCost RepeatS{n_iter = MetaSize k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost RepeatS{n_iter = MetaValue k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost RepeatS{n_iter = MetaName _} = fail "unsupported meta parameter substitution"
stmtCost HoleS{hole} = holeCost hole
stmtCost ForInRangeS{iter_lim = MetaSize k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost ForInRangeS{iter_lim = MetaValue k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost ForInRangeS{iter_lim = _} = fail "unsupported meta parameter substitution"

procCost ::
  ( Integral sizeT
  , Floating costT
  , HoleCost holeT costT
  , m ~ CostCalculator holeT sizeT costT
  ) =>
  Ident ->
  m costT
procCost name = get_cached_cost <|> calc_cost
 where
  get_cached_cost = use (at name) >>= lift
  calc_cost = do
    ProcDef{proc_body_or_tick} <- view $ procCtx . Ctx.at name . unsafeFromJust (printf "could not find predicate %s" name)
    cost <- case proc_body_or_tick of
      Right proc_body -> stmtCost proc_body
      Left tick -> pure tick
    at name ?= cost
    return cost

programCost ::
  (Integral sizeT, Floating costT, HoleCost holeT costT) =>
  Program holeT sizeT costT ->
  (costT, CostMap costT)
programCost Program{proc_defs, stmt} =
  let env = proc_defs
      mres = runMyReaderStateT (stmtCost stmt) env Map.empty
   in case mres of
        Nothing -> error "could not compute cost!"
        Just res -> res
