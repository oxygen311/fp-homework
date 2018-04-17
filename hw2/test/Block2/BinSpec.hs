module Block2.BinSpec (main, spec) where

import           Block2.Bin
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  -- Задание 1: Бинарные последовательности
  describe "bin" $ do
    it "empty list on negative values" $
      bin (-5) `shouldBe` []

    it "list with 1 empty value on 0 (2^0 = 1)" $
      bin 0 `shouldBe` [[]]

    it "test on 1" $
      bin 1 `shouldBe` [[0],[1]]

    it "test on 2" $
      bin 2 `shouldBe` [[0,0],[1,0],[0,1],[1,1]]

    it "test on 3" $
      bin 3 `shouldBe` [[0,0,0],[1,0,0],[0,1,0],[1,1,0],[0,0,1],[1,0,1],[0,1,1],[1,1,1]]

    it "test on 5" $
      length (bin 5) `shouldBe` ((2  ^ 5) :: Int)

    it "test on 10" $
      length (bin 10) `shouldBe` ((2  ^ 10) :: Int)

    it "test on 20" $
      length (bin 15) `shouldBe` ((2  ^ 15) :: Int)
