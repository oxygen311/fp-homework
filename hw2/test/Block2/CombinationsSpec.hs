module Block2.CombinationsSpec (main, spec) where

import           Block2.Combinations
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  -- Задание 2: Сочетания
  describe "combinations" $ do
    it "empty list on negative values" $
      combinations (-1) (-1) `shouldBe` []

    it "empty list on first negative value" $
      combinations (-1) 10 `shouldBe` []

    it "empty list on second negative value" $
      combinations 10 (-1) `shouldBe` []

    it "empty list if k > n" $
      combinations 2 10 `shouldBe` []

    it "test on 4 2" $
      combinations 4 2 `shouldBe` [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]

    it "test on 3 2" $
      combinations 3 2 `shouldBe` [[1,2],[1,3],[2,3]]

    it "test on 2 2" $
      combinations 2 2 `shouldBe` [[1,2]]

    it "test on 8 4" $
      combinations 5 3 `shouldBe` [[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
