{-# LANGUAGE DuplicateRecordFields #-}
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

import Control.Monad.Except (throwError)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Void (Void, absurd)
import Lens.Micro.GHC
import Lens.Micro.Mtl
import Text.Printf (printf)

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.CQPL.Syntax
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Cache the costs of each procedure
type CostMap costT = Map.Map Ident costT

-- | Environment: the list of procedures, and a mapping from holes to cost.
type CostEnv holeT sizeT costT = ProcCtx holeT sizeT costT

-- | Monad to compute unitary cost.
type CostCalculator holeT sizeT costT =
  MyReaderStateT
    (CostEnv holeT sizeT costT)
    (CostMap costT)
    (Either String)

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
stmtCost UCallS{uproc_id} = procCost uproc_id
stmtCost (USeqS ss) = sum <$> mapM stmtCost ss
stmtCost URepeatS{n_iter = P.MetaSize k, uloop_body} = (fromIntegral k *) <$> stmtCost uloop_body
stmtCost URepeatS{n_iter = P.MetaValue k, uloop_body} = (fromIntegral k *) <$> stmtCost uloop_body
stmtCost URepeatS{n_iter = P.MetaName _} = throwError "unsupported meta parameter substitution"
stmtCost UHoleS{uhole} = holeCost uhole
stmtCost UForInRangeS{iter_lim = P.MetaSize k, uloop_body} = (fromIntegral k *) <$> stmtCost uloop_body
stmtCost UForInRangeS{iter_lim = P.MetaValue k, uloop_body} = (fromIntegral k *) <$> stmtCost uloop_body
stmtCost UForInRangeS{iter_lim = _} = throwError "unsupported meta parameter substitution"
stmtCost UWithComputedS{with_ustmt, body_ustmt} = do
  wc <- stmtCost with_ustmt
  bc <- stmtCost body_ustmt
  return $ 2 * wc + bc

procCost ::
  ( Integral sizeT
  , Floating costT
  , HoleCost holeT costT
  , m ~ CostCalculator holeT sizeT costT
  ) =>
  Ident ->
  m costT
procCost name = get_cached_cost >>= maybe calc_cost return
 where
  get_cached_cost = use (at name)
  calc_cost = do
    ProcDef{proc_body} <-
      view (_procCtx . Ctx.at name)
        >>= maybeWithError (printf "could not find predicate %s" name)
    cost <- case proc_body of
      ProcBodyC cproc_body ->
        case cproc_body of
          CProcDecl{ctick} -> pure ctick
          CProcBody{cproc_body_stmt} ->
            throwError "TODO: implement worst-case cost for classical Stmt"
      ProcBodyU uproc_body ->
        case uproc_body of
          UProcDecl{utick} -> pure utick
          UProcBody{uproc_body_stmt} -> stmtCost uproc_body_stmt
    at name ?= cost
    return cost

programCost ::
  (Integral sizeT, Floating costT, HoleCost holeT costT) =>
  Program holeT sizeT costT ->
  (costT, CostMap costT)
programCost Program{proc_defs} = fromRight (error "could not compute cost") $ do
  -- TODO support cost for procs
  let env = proc_defs
  runMyReaderStateT (procCost "main") env Map.empty
