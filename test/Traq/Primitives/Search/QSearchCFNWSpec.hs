{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Search.QSearchCFNWSpec (spec) where

import Control.Monad.RWS (RWST (..), evalRWST)

import Lens.Micro.GHC

import Traq.Data.Default

import qualified Traq.Analysis as A
import Traq.Analysis.CostModel.QueryCost (SimpleQueryCost (..))
import qualified Traq.CPL as CPL
import Traq.Prelude
import Traq.Primitives.Search.QSearchCFNW
import qualified Traq.QPL as QPL
import qualified Traq.Utils.Printing as PP

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen (genDouble)
import TestHelpers

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

execRWT :: (Monad m, Monoid w) => r -> RWST r w () m a -> m w
execRWT r m = snd <$> evalRWST m r ()

data SearchParams = SearchParams {space_size :: Int, precision :: A.FailProb Double}
  deriving (Show, Eq, Read)

instance Arbitrary SearchParams where
  arbitrary = sized $ \n -> do
    space_size <- chooseInt (1, n + 1)
    precision <- A.failProb <$> genDouble
    return SearchParams{space_size, precision}

spec :: Spec
spec = do
  describe "Grover circuit" $ do
    it "for simple values" $ do
      let n = 10 :: Int
      let eps = A.failProb (0.001 :: Double)
      let pred_caller c x b = QPL.UCallS{QPL.uproc_id = "Oracle", QPL.dagger = False, QPL.qargs = [c, x, b]}
      let lenv = default_
      let lctx = default_
      circ <-
        expectRight $
          algoQSearchZalka @CPL.Core' eps "output_bit" "elem"
            & execRWT UQSearchEnv{search_arg_type = CPL.Fin n, pred_call_builder = pred_caller}
            & (\m -> runRWST m lenv lctx)
            <&> (QPL.USeqS . fst3)
      PP.toCodeString circ `shouldSatisfy` (not . null)

  describe "QSearch_Zalka circuit" $ do
    let qsearch_env n =
          UQSearchEnv
            { search_arg_type = CPL.Fin n
            , pred_call_builder = \c x b ->
                QPL.UCallS{QPL.uproc_id = "Oracle", QPL.dagger = False, QPL.qargs = [c, x, b]}
            }

    prop "matches cost" $ \params -> do
      let n = space_size params
      let eps = precision params
      let compile_config = default_
      (n > 2) ==> do
        ss <-
          algoQSearchZalka @(QSearchCFNW SizeT Double) eps "result" "elem"
            & execRWT (qsearch_env n)
            & (\m -> runRWST m compile_config default_)
            <&> fst3
            & expectRight

        let uprog =
              QPL.Program
                [ QPL.ProcDef
                    { QPL.info_comment = ""
                    , QPL.proc_name = "Oracle"
                    , QPL.proc_meta_params = []
                    , QPL.proc_param_types = []
                    , QPL.proc_body = QPL.ProcBodyU QPL.UProcDecl
                    }
                , QPL.ProcDef
                    { QPL.info_comment = ""
                    , QPL.proc_name = "main"
                    , QPL.proc_meta_params = []
                    , QPL.proc_param_types = undefined
                    , QPL.proc_body =
                        QPL.ProcBodyU $
                          QPL.UProcBody
                            { QPL.uproc_body_stmt = QPL.USeqS ss
                            , QPL.uproc_param_names = undefined
                            , QPL.uproc_param_tags = undefined
                            }
                    }
                ]

        let actual_cost = getCost . fst $ QPL.programCost @_ @(SimpleQueryCost Double) uprog
        let formula_cost = 2 * _QSearchZalka n eps
        actual_cost `shouldSatisfy` (<= formula_cost)
