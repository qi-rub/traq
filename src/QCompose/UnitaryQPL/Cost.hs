module QCompose.UnitaryQPL.Cost (
  stmtCost,
  procCost,
  programCost,

  -- * types
  ProcCtx,
  CostMap,
  CostCalculator,
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
type CostEnv holeT sizeT costT = (ProcCtx holeT sizeT costT, holeT -> costT)

-- | Monad to compute unitary cost.
type CostCalculator holeT sizeT costT = MyReaderStateT (CostEnv holeT sizeT costT) (CostMap costT) Maybe

stmtCost :: (Integral sizeT, Show costT, Floating costT) => Stmt holeT sizeT costT -> CostCalculator holeT sizeT costT costT
stmtCost SkipS = return 0
stmtCost UnitaryS{} = return 0
stmtCost CallS{proc_id} = procCost proc_id
stmtCost (SeqS ss) = sum <$> mapM stmtCost ss
stmtCost (RepeatS k s) = (fromIntegral k *) <$> stmtCost s
stmtCost HoleS{hole} = view _2 <*> pure hole

procCost ::
  (Integral sizeT, Show costT, Floating costT) =>
  Ident ->
  CostCalculator holeT sizeT costT costT
procCost name = (use (at name) >>= lift) <|> calc_cost
 where
  calc_cost = do
    ProcDef{mproc_body} <- view $ _1 . Ctx.at name . singular _Just
    proc_body <- lift mproc_body
    cost <- stmtCost proc_body
    at name ?= cost
    return cost

programCost ::
  (Integral sizeT, Show costT, Floating costT) =>
  (holeT -> costT) ->
  Program holeT sizeT costT ->
  (costT, CostMap costT)
programCost holeCost Program{proc_defs, stmt} =
  let env = (proc_defs, holeCost)
   in runMyReaderStateT (stmtCost stmt) env Map.empty ^. singular _Just
