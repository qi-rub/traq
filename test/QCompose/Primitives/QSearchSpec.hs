module QCompose.Primitives.QSearchSpec (spec) where

import Lens.Micro

import QCompose.Control.Monad
import qualified QCompose.Data.Context as Ctx
import QCompose.ProtoLang (VarType (..))
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
        let pred_caller x b = UQPL.CallS{UQPL.proc_id = "Oracle", UQPL.dagger = False, UQPL.args = [x, b]}
        let lenv = (undefined, "Oracle")
        let lctx = (mempty, Ctx.empty)
        circ <-
          expectRight $
            algoQSearchZalka (Fin n) pred_caller eps "output_bit"
              & execMyWriterT
              & (\m -> evalMyReaderStateT m lenv lctx)
              <&> UQPL.SeqS
        toCodeString circ `shouldSatisfy` (not . null)
