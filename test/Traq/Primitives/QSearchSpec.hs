module Traq.Primitives.QSearchSpec (spec) where

import Lens.Micro.GHC

import Traq.Control.Monad
import Traq.Data.Default

import qualified Traq.ProtoLang as P
import qualified Traq.UnitaryQPL as UQPL
import Traq.Utils.Printing

import Traq.Primitives.Search.QSearchCFNW

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "Grover circuit" $ do
    describe "QSearch_Zalka circuit" $ do
      it "for simple values" $ do
        let n = 10 :: Int
        let eps = 0.001 :: Float
        let pred_caller c x b = UQPL.UCallS{UQPL.proc_id = "Oracle", UQPL.dagger = False, UQPL.args = [c, x, b]}
        let lenv = default_ & (P._unitaryTicks . at "Oracle" ?~ 1)
        let lctx = default_
        circ <-
          expectRight $
            algoQSearchZalka eps "output_bit"
              & execMyReaderWriterT UQSearchEnv{search_arg_type = P.Fin n, pred_call_builder = pred_caller}
              & (\m -> evalMyReaderStateT m lenv lctx)
              <&> UQPL.USeqS
        toCodeString circ `shouldSatisfy` (not . null)
