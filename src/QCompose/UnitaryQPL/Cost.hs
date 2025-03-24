module QCompose.UnitaryQPL.Cost where

import Control.Monad.RWS (RWS, runRWS)
import qualified Data.Map as Map
import Lens.Micro
import Lens.Micro.Mtl

import QCompose.Prelude
import QCompose.UnitaryQPL.Syntax

type ProcCtx a = Map.Map Ident (ProcDef a)
type CostMap = Map.Map Ident Complexity

type CostCalculator a = RWS (ProcCtx a) () CostMap

unitaryCost :: Unitary a -> CostCalculator a Complexity
unitaryCost Oracle = return 1
unitaryCost (BlackBoxU QSearchBB{pred_name, n_pred_calls}) = do
  pred_cost <- procCost pred_name
  return $ n_pred_calls * pred_cost
unitaryCost (BlackBoxU (BlackBox _)) = error "cannot compute cost of blackbox"
unitaryCost _ = return 0

stmtCost :: Stmt a -> CostCalculator a Complexity
stmtCost SkipS = return 0
stmtCost (SeqS ss) = sum <$> mapM stmtCost ss
stmtCost UnitaryS{unitary} = unitaryCost unitary
stmtCost CallS{proc_id} = procCost proc_id

procCost :: Ident -> CostCalculator a Complexity
procCost name = do
  mCost <- use $ at name
  case mCost of
    Just cost -> return cost
    Nothing -> do
      ProcDef{proc_body} <- view $ at name . singular _Just
      cost <- stmtCost proc_body
      at name ?= cost
      return cost

programCost :: Program a -> (Complexity, CostMap)
programCost Program{proc_defs, stmt} = (cost, proc_costs)
 where
  (cost, proc_costs, _) = runRWS (stmtCost stmt) proc_map Map.empty
  proc_map = Map.fromList $ [(proc_name p, p) | p <- proc_defs]
