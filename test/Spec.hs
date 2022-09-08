import Universum
import qualified Universum.Unsafe as Unsafe
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      Unsafe.head [23 ..] `shouldBe` (23 :: Int)
