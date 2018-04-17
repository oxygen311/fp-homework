module Block2.PermutationsSpec (main, spec) where

import           Block2.Permutations
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  -- Задание 3: Перестановки
  describe "permutations" $ do
    it "test on [1]" $
      permutations [1] `shouldBe` [[1]]

    it "test on [1, 2]" $
      permutations [1, 2] `shouldBe` [[1,2],[2,1]]

    it "test on [1..3]" $
      permutations [1..3] `shouldBe` [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

    it "test on [1..4]" $
      length (permutations [1..4]) `shouldBe` product [1..4]

    it "test on [1..5]" $
      length (permutations [1..5]) `shouldBe` product [1..5]
