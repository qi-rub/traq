{-# LANGUAGE TypeApplications #-}

module Traq.Primitives.Search.QSearchCFNWSpec (spec) where

import qualified Data.Map as Map
import Lens.Micro.GHC
import Lens.Micro.Mtl

import Traq.Control.Monad
import qualified Traq.Data.Context as Ctx
import Traq.Data.Default
import Traq.Prelude

import Traq.Primitives.Search.QSearchCFNW
import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen (genDouble)
import TestHelpers

data SearchParams = SearchParams {space_size :: Int, precision :: Double}
  deriving (Show, Eq, Read)

instance Arbitrary SearchParams where
  arbitrary = sized $ \n -> do
    space_size <- chooseInt (1, n + 1)
    precision <- genDouble
    return SearchParams{space_size, precision}

spec :: Spec
spec = do
  describe "QSearch_Zalka circuit" $ do
    let qsearch_env n =
          UQSearchEnv
            { search_arg_type = P.Fin n
            , pred_call_builder = \c x b ->
                UQPL.UCallS{UQPL.proc_id = "Oracle", UQPL.dagger = False, UQPL.args = [c, x, b]}
            }

    prop "matches cost" $ \params -> do
      let n = space_size params
      let delta = precision params
      let compile_config = default_ & (P._unitaryTicks . at "Oracle" ?~ 1.0)
      (n > 1) ==> do
        (ss, []) <-
          algoQSearchZalka @QSearchCFNW @_ @SizeT delta "result"
            & execMyReaderWriterT (qsearch_env n)
            & (\act -> evalMyReaderWriterStateT act compile_config default_)
            & expectRight

        let uprog =
              UQPL.Program
                { UQPL.stmt = UQPL.USeqS ss
                , UQPL.proc_defs =
                    Ctx.singleton
                      "Oracle"
                      UQPL.UProcDef
                        { UQPL.proc_name = "Oracle"
                        , UQPL.proc_meta_params = []
                        , UQPL.proc_params = []
                        , UQPL.proc_body_or_tick = Left (1.0 :: Double)
                        }
                }

        let (actual_cost, _) = UQPL.programCost @_ @Double uprog
        let formula_cost = _QSearchZalka n delta
        actual_cost `shouldSatisfy` (<= formula_cost)
