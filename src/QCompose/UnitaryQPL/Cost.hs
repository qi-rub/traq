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
import Lens.Micro.GHC
import Lens.Micro.Mtl

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.UnitaryQPL.Syntax

-- | Cache the costs of each procedure
type CostMap costT = Map.Map Ident costT

-- | Environment: the list of procedures, and a mapping from holes to cost.
type CostEnv holeT sizeT costT = ProcCtx holeT sizeT

procCtx :: Lens' (CostEnv holeT sizeT costT) (ProcCtx holeT sizeT)
procCtx = id

-- | Monad to compute unitary cost.
type CostCalculator holeT sizeT costT = MyReaderStateT (CostEnv holeT sizeT costT) (CostMap costT) Maybe

-- | Compute the cost of a placeholder (hole) program.
class HoleCost holeT costT where
  holeCost :: forall sizeT. (Integral sizeT, Floating costT) => holeT -> CostCalculator holeT sizeT costT costT

stmtCost :: (Integral sizeT, Floating costT, HoleCost holeT costT) => Stmt holeT sizeT -> CostCalculator holeT sizeT costT costT
stmtCost SkipS = return 0
stmtCost UnitaryS{} = return 0
stmtCost CallS{proc_id} = procCost proc_id
stmtCost (SeqS ss) = sum <$> mapM stmtCost ss
stmtCost (RepeatS k s) = (fromIntegral k *) <$> stmtCost s
stmtCost HoleS{hole} = holeCost hole

procCost ::
  (Integral sizeT, Floating costT, HoleCost holeT costT) =>
  Ident ->
  CostCalculator holeT sizeT costT costT
procCost name = (use (at name) >>= lift) <|> calc_cost_of_oracle <|> calc_cost
 where
  calc_cost_of_oracle = do
    ProcDef{is_oracle} <- view $ procCtx . Ctx.at name . singular _Just
    cost <- lift $ if is_oracle then Just 1 else Nothing
    at name ?= cost
    return cost
  calc_cost = do
    ProcDef{mproc_body} <- view $ procCtx . Ctx.at name . singular _Just
    proc_body <- lift mproc_body
    cost <- stmtCost proc_body
    at name ?= cost
    return cost

programCost ::
  (Integral sizeT, Floating costT, HoleCost holeT costT) =>
  Program holeT sizeT ->
  (costT, CostMap costT)
programCost Program{proc_defs, stmt} =
  let env = proc_defs
      mres = runMyReaderStateT (stmtCost stmt) env Map.empty
   in case mres of
        Nothing -> error "could not compute cost!"
        Just res -> res
