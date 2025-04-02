module QCompose.UnitaryQPL.Cost where

import Control.Monad.RWS (RWS, runRWS)
import qualified Data.Map as Map
import Lens.Micro
import Lens.Micro.Mtl

import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.UnitaryQPL.Syntax

type ProcCtx a costT = Ctx.Context (ProcDef a costT)
type CostMap costT = Map.Map Ident costT

type CostCalculator a costT = RWS (ProcCtx a costT) () (CostMap costT)

unitaryCost :: (Show costT, Floating costT) => Unitary a costT -> CostCalculator a costT costT
unitaryCost Oracle = return 1
unitaryCost (BlackBoxU QSearchBB{pred_name, n_pred_calls}) = do
  pred_cost <- procCost pred_name
  return $ n_pred_calls * pred_cost
unitaryCost (BlackBoxU (BlackBox _)) = error "cannot compute cost of blackbox"
unitaryCost _ = return 0

stmtCost :: (Show costT, Floating costT) => Stmt a costT -> CostCalculator a costT costT
stmtCost SkipS = return 0
stmtCost (SeqS ss) = sum <$> mapM stmtCost ss
stmtCost UnitaryS{unitary} = unitaryCost unitary
stmtCost CallS{proc_id} = procCost proc_id

procCost :: (Show costT, Floating costT) => Ident -> CostCalculator a costT costT
procCost name = do
  mCost <- use $ at name
  case mCost of
    Just cost -> return cost
    Nothing -> do
      ProcDef{proc_body} <- view $ Ctx.at name . singular _Just
      cost <- stmtCost proc_body
      at name ?= cost
      return cost

programCost :: (Show costT, Floating costT) => Program a costT -> (costT, CostMap costT)
programCost Program{proc_defs, stmt} = (cost, proc_costs)
 where
  (cost, proc_costs, _) = runRWS (stmtCost stmt) proc_map Map.empty
  proc_map = Ctx.fromList $ [(proc_name p, p) | p <- proc_defs]
