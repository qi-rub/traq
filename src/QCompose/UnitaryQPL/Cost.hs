module QCompose.UnitaryQPL.Cost (
  unitaryCost,
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
import Lens.Micro
import Lens.Micro.GHC ()
import Lens.Micro.Mtl

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx

import QCompose.Prelude
import QCompose.UnitaryQPL.Syntax

type CostMap costT = Map.Map Ident costT

type CostCalculator sizeT costT = MyReaderStateT (ProcCtx sizeT costT) (CostMap costT) Maybe

unitaryCost ::
  (Integral sizeT, Show costT, Floating costT) =>
  Unitary sizeT costT ->
  CostCalculator sizeT costT costT
unitaryCost (BlackBoxU QSearchBB{pred_name, n_pred_calls}) = do
  pred_cost <- procCost pred_name
  return $ n_pred_calls * pred_cost
unitaryCost (BlackBoxU (BlackBox _)) = error "cannot compute cost of blackbox"
unitaryCost _ = return 0

stmtCost :: (Integral sizeT, Show costT, Floating costT) => Stmt sizeT costT -> CostCalculator sizeT costT costT
stmtCost SkipS = return 0
stmtCost (SeqS ss) = sum <$> mapM stmtCost ss
stmtCost UnitaryS{unitary} = unitaryCost unitary
stmtCost CallS{proc_id} = procCost proc_id
stmtCost (RepeatS k s) = (fromIntegral k *) <$> stmtCost s
stmtCost _ = error "no cost"

procCost ::
  (Integral sizeT, Show costT, Floating costT) =>
  Ident ->
  CostCalculator sizeT costT costT
procCost name = (use (at name) >>= lift) <|> calc_cost
 where
  calc_cost = do
    ProcDef{mproc_body} <- view $ Ctx.at name . singular _Just
    proc_body <- lift mproc_body
    cost <- stmtCost proc_body
    at name ?= cost
    return cost

programCost ::
  (Integral sizeT, Show costT, Floating costT) =>
  Program sizeT costT ->
  (costT, CostMap costT)
programCost Program{proc_defs, stmt} =
  runMyReaderStateT (stmtCost stmt) proc_defs Map.empty ^. singular _Just
