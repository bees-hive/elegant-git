module LibSpec where

import           Test.Hspec
import           Universum
import Lib

spec :: Spec
spec = do
  describe "Lib.tst" $ do
    it "returns the first a value 1" $ do
      Lib.tst `shouldBe` 1
