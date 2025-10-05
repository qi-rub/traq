{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Search.QSearchCFNWSpec (spec) where

import Control.Monad.RWS (RWST, evalRWST)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriterT)

import Lens.Micro.GHC

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default

import qualified Traq.CQPL as CQPL
import Traq.CostModel.QueryCost (SimpleQueryCost (..))
import Traq.Prelude
import Traq.Primitives.Search.QSearchCFNW
import qualified Traq.ProtoLang as P
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen (genDouble)
import TestHelpers

execRWT :: (Monad m, Monoid w) => r -> RWST r w () m a -> m w
execRWT r m = snd <$> evalRWST m r ()

data SearchParams = SearchParams {space_size :: Int, precision :: Double}
  deriving (Show, Eq, Read)

instance Arbitrary SearchParams where
  arbitrary = sized $ \n -> do
    space_size <- chooseInt (1, n + 1)
    precision <- genDouble
    return SearchParams{space_size, precision}

spec :: Spec
spec = do
  describe "Grover circuit" $ do
    it "for simple values" $ do
      let n = 10 :: Int
      let eps = 0.001 :: Float
      let pred_caller c x b = CQPL.UCallS{CQPL.uproc_id = "Oracle", CQPL.dagger = False, CQPL.qargs = [c, x, b]}
      let lenv = default_
      let lctx = default_
      circ <-
        expectRight $
          algoQSearchZalka eps "output_bit"
            & execRWT UQSearchEnv{search_arg_type = P.Fin n, pred_call_builder = pred_caller}
            & runWriterT
            <&> fst
            & (runReaderT ?? lenv)
            & (evalStateT ?? lctx)
            <&> CQPL.USeqS
      PP.toCodeString circ `shouldSatisfy` (not . null)

  describe "QSearch_Zalka circuit" $ do
    let qsearch_env n =
          UQSearchEnv
            { search_arg_type = P.Fin n
            , pred_call_builder = \c x b ->
                CQPL.UCallS{CQPL.uproc_id = "Oracle", CQPL.dagger = False, CQPL.qargs = [c, x, b]}
            }

    prop "matches cost" $ \params -> do
      let n = space_size params
      let delta = precision params
      let compile_config = default_
      (n > 1) ==> do
        (ss, []) <-
          algoQSearchZalka @QSearchCFNW @SizeT delta "result"
            & execRWT (qsearch_env n)
            & runWriterT
            & (runReaderT ?? compile_config)
            & (evalStateT ?? default_)
            & expectRight

        let uprog =
              CQPL.Program
                { CQPL.proc_defs =
                    Ctx.fromListWith
                      CQPL.proc_name
                      [ CQPL.ProcDef
                          { CQPL.info_comment = ""
                          , CQPL.proc_name = "Oracle"
                          , CQPL.proc_meta_params = []
                          , CQPL.proc_param_types = []
                          , CQPL.proc_body = CQPL.ProcBodyU CQPL.UProcDecl
                          }
                      , CQPL.ProcDef
                          { CQPL.info_comment = ""
                          , CQPL.proc_name = "main"
                          , CQPL.proc_meta_params = []
                          , CQPL.proc_param_types = undefined
                          , CQPL.proc_body =
                              CQPL.ProcBodyU $
                                CQPL.UProcBody
                                  { CQPL.uproc_body_stmt = CQPL.USeqS ss
                                  , CQPL.uproc_param_names = undefined
                                  , CQPL.uproc_param_tags = undefined
                                  }
                          }
                      ]
                }

        let actual_cost = getCost . fst $ CQPL.programCost @_ @(SimpleQueryCost Double) uprog
        let formula_cost = _QSearchZalka n delta
        actual_cost `shouldSatisfy` (<= formula_cost)
