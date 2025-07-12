{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Cost (
  stmtCost,
  procCost,
  programCost,

  -- * types
  CostMap,
  CostCalculator,

  -- * Holes
  HoleCost (..),
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (lift)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.CQPL.Syntax as CQPL
import Traq.Prelude
import qualified Traq.ProtoLang as P
import Traq.UnitaryQPL.Syntax

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
  UStmt holeT sizeT ->
  m costT
stmtCost USkipS = return 0
stmtCost (UCommentS _) = return 0
stmtCost UnitaryS{} = return 0
stmtCost UCallS{proc_id} = procCost proc_id
stmtCost (USeqS ss) = sum <$> mapM stmtCost ss
stmtCost URepeatS{n_iter = P.MetaSize k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost URepeatS{n_iter = P.MetaValue k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost URepeatS{n_iter = P.MetaName _} = fail "unsupported meta parameter substitution"
stmtCost UHoleS{hole} = holeCost hole
stmtCost UForInRangeS{iter_lim = P.MetaSize k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost UForInRangeS{iter_lim = P.MetaValue k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost UForInRangeS{iter_lim = _} = fail "unsupported meta parameter substitution"
stmtCost UWithComputedS{with_stmt, body_stmt} = do
  wc <- stmtCost with_stmt
  bc <- stmtCost body_stmt
  return $ 2 * wc + bc

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
    UProcDef{proc_body_or_tick} <- view $ procCtx . Ctx.at name . unsafeFromJust (printf "could not find predicate %s" name)
    cost <- case proc_body_or_tick of
      Right proc_body -> stmtCost proc_body
      Left tick -> pure tick
    at name ?= cost
    return cost

programCost ::
  (Integral sizeT, Floating costT, HoleCost holeT costT) =>
  CQPL.Program holeT sizeT costT ->
  (costT, CostMap costT)
programCost CQPL.Program{CQPL.uproc_defs} = fromMaybe (error "could not compute cost") $ do
  -- TODO support cost for procs
  let env = uproc_defs
  UProcDef
    { proc_meta_params = []
    , proc_body_or_tick = Right main_body
    } <-
    uproc_defs ^. Ctx.at "main"
  runMyReaderStateT (stmtCost main_body) env Map.empty
