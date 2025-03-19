module QCompose.UnitaryQPL.Cost where

import Control.Monad.RWS (RWS, runRWS)
import qualified Data.Map as Map
import Lens.Micro
import Lens.Micro.Mtl

import QCompose.Prelude
import QCompose.UnitaryQPL.Syntax

type CostCalculator a = RWS (Map.Map Ident (ProcDef a)) () (Map.Map Ident Complexity)

unitaryCost :: Unitary a -> CostCalculator a Complexity
unitaryCost Oracle = return 1
unitaryCost (BlackBoxU QSearchBB{pred_name, n_pred_calls}) = do
  pred_cost <- procCost pred_name
  return $ 2 * n_pred_calls * pred_cost
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

programCost :: Program a -> Complexity
programCost Program{proc_defs, stmt} = runRWS (stmtCost stmt) proc_map Map.empty ^. _1
 where
  proc_map = Map.fromList $ [(proc_name p, p) | p <- proc_defs]
