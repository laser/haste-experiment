-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Addition

main :: IO ()
main = hspec $ do
  describe "Addition.add" $ do
    it "returns the sum of two Integers" $ do
      add 10 20 `shouldBe` (30 :: Integer)
