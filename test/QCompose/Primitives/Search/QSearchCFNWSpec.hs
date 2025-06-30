{-# LANGUAGE TypeApplications #-}

module QCompose.Primitives.Search.QSearchCFNWSpec (spec) where

import qualified Data.Map as Map
import Lens.Micro.GHC

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import QCompose.Prelude

import QCompose.Primitives.Search.QSearchCFNW
import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL

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
                UQPL.CallS{UQPL.proc_id = "Oracle", UQPL.dagger = False, UQPL.args = [c, x, b]}
            }

    let compile_config = (Ctx.empty, Map.singleton "Oracle" 1.0)
    let compile_ctx = (mempty, Ctx.empty)

    prop "matches cost" $ \params -> do
      let n = space_size params
      let delta = precision params

      n > 1 ==> do
        (ss, []) <-
          algoQSearchZalka @QSearchCFNW @_ @SizeT delta "result"
            & execMyReaderWriterT (qsearch_env n)
            & (\act -> evalMyReaderWriterStateT act compile_config compile_ctx)
            & expectRight

        let uprog =
              UQPL.Program
                { UQPL.stmt = UQPL.SeqS ss
                , UQPL.proc_defs =
                    Ctx.singleton
                      "Oracle"
                      UQPL.ProcDef
                        { UQPL.proc_name = "Oracle"
                        , UQPL.proc_meta_params = []
                        , UQPL.proc_params = []
                        , UQPL.proc_body_or_tick = Left (1.0 :: Double)
                        }
                }

        let (actual_cost, _) = UQPL.programCost @_ @Double uprog
        let formula_cost = _QSearchZalka n delta
        actual_cost `shouldSatisfy` (<= formula_cost)
