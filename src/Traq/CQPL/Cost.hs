{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Cost (
  procCost,
  programCost,

  -- * types
  CostMap,
  CostCalculator,

  -- * Holes
  HoleCost (..),
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
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
  ReaderT
    (CostEnv holeT sizeT costT)
    ( StateT
        (CostMap costT)
        (Either String)
    )

-- | Compute the cost of a placeholder (hole) program.
class HoleCost holeT costT where
  holeCost ::
    forall sizeT m.
    ( Integral sizeT
    , Floating costT
    , Ord costT
    , m ~ CostCalculator holeT sizeT costT
    ) =>
    holeT ->
    m costT

instance HoleCost Void costT where
  holeCost = absurd

ustmtCost ::
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , HoleCost holeT costT
  , m ~ CostCalculator holeT sizeT costT
  ) =>
  UStmt holeT sizeT ->
  m costT
ustmtCost USkipS = return 0
ustmtCost (UCommentS _) = return 0
ustmtCost UnitaryS{} = return 0
ustmtCost UCallS{uproc_id} = procCost uproc_id
ustmtCost (USeqS ss) = sum <$> mapM ustmtCost ss
ustmtCost URepeatS{n_iter = P.MetaSize k, uloop_body} = (fromIntegral k *) <$> ustmtCost uloop_body
ustmtCost URepeatS{n_iter = P.MetaValue k, uloop_body} = (fromIntegral k *) <$> ustmtCost uloop_body
ustmtCost URepeatS{n_iter = P.MetaName _} = throwError "unsupported meta parameter substitution"
ustmtCost UHoleS{uhole} = holeCost uhole
ustmtCost UForInRangeS{iter_lim = P.MetaSize k, uloop_body} = (fromIntegral k *) <$> ustmtCost uloop_body
ustmtCost UForInRangeS{iter_lim = P.MetaValue k, uloop_body} = (fromIntegral k *) <$> ustmtCost uloop_body
ustmtCost UForInRangeS{iter_lim = _} = throwError "unsupported meta parameter substitution"
ustmtCost UWithComputedS{with_ustmt, body_ustmt} = do
  wc <- ustmtCost with_ustmt
  bc <- ustmtCost body_ustmt
  return $ 2 * wc + bc

stmtCost ::
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , HoleCost holeT costT
  , m ~ CostCalculator holeT sizeT costT
  ) =>
  Stmt holeT sizeT ->
  m costT
-- zero-cost statements
stmtCost SkipS = return 0
stmtCost (CommentS _) = return 0
stmtCost AssignS{} = return 0
stmtCost RandomS{} = return 0
stmtCost RandomDynS{} = return 0
-- single statements
stmtCost CallS{fun = FunctionCall p, meta_params = []} = procCost p
stmtCost CallS{fun = UProcAndMeas up, meta_params = []} = procCost up
stmtCost CallS{} = throwError "unsupported cost: proc call with params"
stmtCost (HoleS h) = holeCost h
-- compound statements
stmtCost (SeqS ss) = sum <$> mapM stmtCost ss
stmtCost IfThenElseS{s_true, s_false} = max <$> stmtCost s_true <*> stmtCost s_false
stmtCost RepeatS{n_iter = P.MetaSize k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost RepeatS{n_iter = P.MetaValue k, loop_body} = (fromIntegral k *) <$> stmtCost loop_body
stmtCost RepeatS{} = throwError "unsupported cost"
stmtCost WhileK{} = throwError "unsupported cost"
stmtCost WhileKWithCondExpr{} = throwError "unsupported cost"
stmtCost ForInArray{} = throwError "unsupported cost"

procCost ::
  ( Integral sizeT
  , Floating costT
  , Ord costT
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
          CProcBody{cproc_body_stmt} -> stmtCost cproc_body_stmt
      ProcBodyU uproc_body ->
        case uproc_body of
          UProcDecl{utick} -> pure utick
          UProcBody{uproc_body_stmt} -> ustmtCost uproc_body_stmt
    at name ?= cost
    return cost

programCost ::
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , HoleCost holeT costT
  ) =>
  Program holeT sizeT costT ->
  (costT, CostMap costT)
programCost Program{proc_defs} = fromRight (error "could not compute cost") $ do
  let env = proc_defs
  procCost "main"
    & (runReaderT ?? env)
    & (runStateT ?? mempty)
