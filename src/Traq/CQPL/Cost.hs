{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Cost (
  procCost,
  programCost,

  -- * types
  CostMap,
  CostCalculator,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.CQPL.Syntax
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Cache the costs of each procedure
type CostMap costT = Map.Map Ident costT

-- | Environment: the list of procedures.
type CostEnv sizeT costT = ProcCtx sizeT costT

-- | Monad to compute unitary cost.
type CostCalculator sizeT costT =
  ReaderT
    (CostEnv sizeT costT)
    ( StateT
        (CostMap costT)
        (Either String)
    )

ustmtCost ::
  ( Integral sizeT
  , Floating costT
  , Ord costT
  , m ~ CostCalculator sizeT costT
  ) =>
  UStmt sizeT ->
  m costT
ustmtCost USkipS = return 0
ustmtCost (UCommentS _) = return 0
ustmtCost UnitaryS{} = return 0
ustmtCost UCallS{uproc_id} = procCost uproc_id
ustmtCost (USeqS ss) = sum <$> mapM ustmtCost ss
ustmtCost URepeatS{n_iter = P.MetaSize k, uloop_body} = (fromIntegral k *) <$> ustmtCost uloop_body
ustmtCost URepeatS{n_iter = P.MetaValue k, uloop_body} = (fromIntegral k *) <$> ustmtCost uloop_body
ustmtCost URepeatS{n_iter = P.MetaName _} = throwError "unsupported meta parameter substitution"
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
  , m ~ CostCalculator sizeT costT
  ) =>
  Stmt sizeT ->
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
  , m ~ CostCalculator sizeT costT
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
  forall sizeT costT.
  ( Integral sizeT
  , Floating costT
  , Ord costT
  ) =>
  Program sizeT costT ->
  (costT, CostMap costT)
programCost Program{proc_defs} = fromRight (error "could not compute cost") $ do
  let env = proc_defs
  procCost "main"
    & (runReaderT ?? env)
    & (runStateT ?? mempty)
