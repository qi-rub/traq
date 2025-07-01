module QCompose.Primitives.QSearchSpec (spec) where

import Lens.Micro.GHC

import QCompose.Control.Monad
import QCompose.Data.Default

import qualified QCompose.ProtoLang as P
import qualified QCompose.UnitaryQPL as UQPL
import QCompose.Utils.Printing

import QCompose.Primitives.Search.QSearchCFNW

import Test.Hspec
import TestHelpers

spec :: Spec
spec = do
  describe "Grover circuit" $ do
    describe "QSearch_Zalka circuit" $ do
      it "for simple values" $ do
        let n = 10 :: Int
        let eps = 0.001 :: Float
        let pred_caller c x b = UQPL.CallS{UQPL.proc_id = "Oracle", UQPL.dagger = False, UQPL.args = [c, x, b]}
        let lenv = default_ & (P._unitaryTicks . at "Oracle" ?~ 1)
        let lctx = default_
        circ <-
          expectRight $
            algoQSearchZalka eps "output_bit"
              & execMyReaderWriterT UQSearchEnv{search_arg_type = P.Fin n, pred_call_builder = pred_caller}
              & (\m -> evalMyReaderStateT m lenv lctx)
              <&> UQPL.SeqS
        toCodeString circ `shouldSatisfy` (not . null)
