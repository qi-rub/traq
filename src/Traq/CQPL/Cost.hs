{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Traq.CQPL.Cost (
  programCost,

  -- * types
  CostMap,
  CostCalculator,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, runStateT)
import qualified Data.Map as Map
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import qualified Traq.Analysis.CostModel.Class as C
import Traq.CQPL.Syntax
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Cache the costs of each procedure
type CostMap c = Map.Map Ident c

-- | Environment: the list of procedures.
type CostEnv size = ProcCtx size

-- | Monad to compute unitary cost.
type CostCalculator size c =
  ReaderT
    (CostEnv size)
    ( StateT
        (CostMap c)
        (Either String)
    )

class HasCost t where
  cost ::
    forall size prec costT m.
    ( size ~ SizeType t
    , Integral size
    , C.CostModel costT
    , prec ~ PrecType costT
    , Ord costT
    , Floating prec
    , m ~ CostCalculator size costT
    ) =>
    t ->
    m costT

instance HasCost (UStmt size) where
  cost USkipS = return Alg.zero
  cost (UCommentS _) = return Alg.zero
  cost UnitaryS{} = return Alg.zero
  cost UCallS{uproc_id} = cachedProcCost uproc_id
  cost (USeqS ss) = Alg.sum <$> mapM cost ss
  cost URepeatS{n_iter = P.MetaSize k, uloop_body} = Alg.sinnum (fromIntegral k) <$> cost uloop_body
  cost URepeatS{n_iter = P.MetaName _} = return Alg.zero
  cost UForInRangeS{iter_lim = P.MetaSize k, uloop_body} = Alg.sinnum (fromIntegral k) <$> cost uloop_body
  cost UForInRangeS{iter_lim = P.MetaName _} = return Alg.zero
  cost UWithComputedS{with_ustmt, body_ustmt} = do
    wc <- cost with_ustmt
    bc <- cost body_ustmt
    return $ wc Alg.+ wc Alg.+ bc

instance HasCost (Stmt size) where
  -- zero-cost statements
  cost SkipS = return Alg.zero
  cost (CommentS _) = return Alg.zero
  cost AssignS{} = return Alg.zero
  cost RandomS{} = return Alg.zero
  cost RandomDynS{} = return Alg.zero
  -- single statements
  cost CallS{fun = FunctionCall p} = cachedProcCost p
  cost CallS{fun = UProcAndMeas up} = cachedProcCost up
  -- compound statements
  cost (SeqS ss) = Alg.sum <$> mapM cost ss
  cost IfThenElseS{s_true, s_false} = max <$> cost s_true <*> cost s_false
  cost RepeatS{n_iter = P.MetaSize k, loop_body} = Alg.sinnum (fromIntegral k) <$> cost loop_body
  cost RepeatS{} = throwError "unsupported cost"
  cost WhileK{n_iter = P.MetaSize k, loop_body} = Alg.sinnum (fromIntegral k) <$> cost loop_body
  cost WhileK{} = throwError "unsupported cost"
  cost WhileKWithCondExpr{n_iter = P.MetaSize k, loop_body} = Alg.sinnum (fromIntegral k) <$> cost loop_body
  cost WhileKWithCondExpr{} = throwError "unsupported cost"
  cost ForInArray{loop_values, loop_body} = Alg.sinnum (fromIntegral (length loop_values)) <$> cost loop_body
  cost ForInRangeS{iter_lim = P.MetaSize k, loop_body} = Alg.sinnum (fromIntegral k) <$> cost loop_body
  cost ForInRangeS{} = throwError "unsupported cost"

instance HasCost (ProcDef size) where
  cost ProcDef{proc_name, proc_body = ProcBodyC CProcDecl} = pure $ C.query C.Classical proc_name
  cost ProcDef{proc_name, proc_body = ProcBodyU UProcDecl} = pure $ C.query C.Unitary proc_name
  cost ProcDef{proc_body = ProcBodyC CProcBody{cproc_body_stmt}} = cost cproc_body_stmt
  cost ProcDef{proc_body = ProcBodyU UProcBody{uproc_body_stmt}} = cost uproc_body_stmt

cachedProcCost ::
  ( Integral size
  , C.CostModel c
  , prec ~ PrecType c
  , Ord c
  , Floating prec
  , m ~ CostCalculator size c
  ) =>
  Ident ->
  m c
cachedProcCost name = get_cached_cost >>= maybe calc_cost return
 where
  get_cached_cost = use (at name)
  calc_cost = do
    p <-
      view (_procCtx . Ctx.at name)
        >>= maybeWithError (printf "could not find proc %s" name)
    c <- cost p
    at name ?= c
    return c

programCost ::
  forall size c prec.
  ( Integral size
  , C.CostModel c
  , prec ~ PrecType c
  , Ord c
  , Floating prec
  ) =>
  Program size ->
  (c, CostMap c)
programCost (Program ps) =
  either (\e -> error $ "could not compute cost: " ++ e) id $ do
    let env = Ctx.fromListWith proc_name ps
    let main_name = proc_name $ last ps
    cachedProcCost main_name
      & (runReaderT ?? env)
      & (runStateT ?? mempty)
