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
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map
import Text.Printf (printf)

import Lens.Micro.GHC
import Lens.Micro.Mtl
import qualified Numeric.Algebra as Alg

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx

import Traq.CQPL.Syntax
import qualified Traq.CostModel.Class as C
import Traq.Prelude
import qualified Traq.ProtoLang as P

-- | Cache the costs of each procedure
type CostMap c = Map.Map Ident c

-- | Environment: the list of procedures.
type CostEnv sizeT = ProcCtx sizeT

-- | Monad to compute unitary cost.
type CostCalculator sizeT c =
  ReaderT
    (CostEnv sizeT)
    ( StateT
        (CostMap c)
        (Either String)
    )

class HasCost t where
  cost ::
    forall sizeT precT costT m.
    ( sizeT ~ SizeType t
    , Integral sizeT
    , C.CostModel costT
    , precT ~ PrecType costT
    , Ord costT
    , Floating precT
    , m ~ CostCalculator sizeT costT
    ) =>
    t ->
    m costT

instance HasCost (UStmt sizeT) where
  cost USkipS = return Alg.zero
  cost (UCommentS _) = return Alg.zero
  cost UnitaryS{} = return Alg.zero
  cost UCallS{uproc_id} = cachedProcCost uproc_id
  cost (USeqS ss) = Alg.sum <$> mapM cost ss
  cost URepeatS{n_iter = P.MetaSize k, uloop_body} = Alg.sinnum (fromIntegral k) <$> cost uloop_body
  cost URepeatS{n_iter = P.MetaValue k, uloop_body} = Alg.sinnum (fromIntegral k) <$> cost uloop_body
  cost URepeatS{n_iter = P.MetaName _} = throwError "unsupported meta parameter substitution"
  cost UForInRangeS{iter_lim = P.MetaSize k, uloop_body} = Alg.sinnum (fromIntegral k) <$> cost uloop_body
  cost UForInRangeS{iter_lim = P.MetaValue k, uloop_body} = Alg.sinnum (fromIntegral k) <$> cost uloop_body
  cost UForInRangeS{iter_lim = _} = throwError "unsupported meta parameter substitution"
  cost UWithComputedS{with_ustmt, body_ustmt} = do
    wc <- cost with_ustmt
    bc <- cost body_ustmt
    return $ wc Alg.+ wc Alg.+ bc

instance HasCost (Stmt sizeT) where
  -- zero-cost statements
  cost SkipS = return Alg.zero
  cost (CommentS _) = return Alg.zero
  cost AssignS{} = return Alg.zero
  cost RandomS{} = return Alg.zero
  cost RandomDynS{} = return Alg.zero
  -- single statements
  cost CallS{fun = FunctionCall p, meta_params = []} = cachedProcCost p
  cost CallS{fun = UProcAndMeas up, meta_params = []} = cachedProcCost up
  cost CallS{} = throwError "unsupported cost: proc call with params"
  -- compound statements
  cost (SeqS ss) = Alg.sum <$> mapM cost ss
  cost IfThenElseS{s_true, s_false} = max <$> cost s_true <*> cost s_false
  cost RepeatS{n_iter = P.MetaSize k, loop_body} = Alg.sinnum (fromIntegral k) <$> cost loop_body
  cost RepeatS{n_iter = P.MetaValue k, loop_body} = Alg.sinnum (fromIntegral k) <$> cost loop_body
  cost RepeatS{} = throwError "unsupported cost"
  cost WhileK{} = throwError "unsupported cost"
  cost WhileKWithCondExpr{} = throwError "unsupported cost"
  cost ForInArray{} = throwError "unsupported cost"

instance HasCost (ProcDef sizeT) where
  cost ProcDef{proc_name, proc_body = ProcBodyC CProcDecl} = pure $ C.query C.Classical proc_name
  cost ProcDef{proc_name, proc_body = ProcBodyU UProcDecl} = pure $ C.query C.Unitary proc_name
  cost ProcDef{proc_body = ProcBodyC CProcBody{cproc_body_stmt}} = cost cproc_body_stmt
  cost ProcDef{proc_body = ProcBodyU UProcBody{uproc_body_stmt}} = cost uproc_body_stmt

cachedProcCost ::
  ( Integral sizeT
  , C.CostModel c
  , precT ~ PrecType c
  , Ord c
  , Floating precT
  , m ~ CostCalculator sizeT c
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
  forall sizeT c precT.
  ( Integral sizeT
  , C.CostModel c
  , precT ~ PrecType c
  , Ord c
  , Floating precT
  ) =>
  Program sizeT ->
  (c, CostMap c)
programCost Program{proc_defs} =
  either (\e -> error $ "could not compute cost: " ++ e) id $ do
    let env = proc_defs
    let main_name = proc_name $ last $ toList proc_defs
    cachedProcCost main_name
      & (runReaderT ?? env)
      & (runStateT ?? mempty)
