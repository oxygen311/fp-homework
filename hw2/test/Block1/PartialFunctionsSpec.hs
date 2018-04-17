module Block1.PartialFunctionsSpec (main, spec) where

import           Block1.PartialFunctions
import           Control.Category        (Category (..))
import           Test.Hspec

main :: IO ()
main = hspec spec

testPartFunc :: Int -> Maybe Int
testPartFunc x
    | even x    = Just x
    | otherwise = Nothing

testPartFunc' :: Int -> Maybe Int
testPartFunc' x
    | even x    = Just $ x - 10
    | otherwise = Nothing

testTotalFunc :: Int -> Int
testTotalFunc = (+2)

testTotalFunc' :: Int -> Int
testTotalFunc' = (+42)

spec :: Spec
spec = do
  -- Задание 2: Частичные функции
  describe "apply" $ do
    it "apply partial at defined case" $
      apply (partial testPartFunc) 42 `shouldBe` Just 42

    it "apply partial at undefined case" $
      apply (partial testPartFunc) 13 `shouldBe` Nothing

    it "apply total" $
      apply (total  testTotalFunc) 42 `shouldBe` Just (42 + 2)


  describe "applyOrElse" $ do
    it "applyOrElse partial at defined case" $
      applyOrElse (partial testPartFunc) 42 0 `shouldBe` 42

    it "applyOrElse partial at undefined case" $
      applyOrElse (partial testPartFunc) 13 0 `shouldBe` 0

    it "applyOrElse total" $
      applyOrElse (total  testTotalFunc) 42 0 `shouldBe` 42 + 2


  describe "withDefault" $ do
    it "apply withDefault from partial at defined case" $
      apply (withDefault (partial testPartFunc) 0) 42 `shouldBe` Just 42

    it "apply withDefault from partial at undefined case" $
      apply (withDefault (partial testPartFunc) 0) 13 `shouldBe` Just 0

    it "apply withDefault from total should change nothing" $
      apply (withDefault (total  testTotalFunc) 0) 42 `shouldBe` Just (42 + 2)


  describe "isDefinedAt" $ do
    it "isDefinedAt on partial function that returns Just" $
      isDefinedAt (partial testPartFunc) 42 `shouldBe` True

    it "isDefinedAt on partial function that returns Nothing" $
      isDefinedAt (partial testPartFunc) 13 `shouldBe` False

    it "isDefinedAt on total function should always return True" $
      isDefinedAt (total testTotalFunc)  13 `shouldBe` True


  let funPartTot = orElse (partial testPartFunc) (total testTotalFunc )
      funTotTot' = orElse (total  testTotalFunc) (total testTotalFunc') in
    describe "orElse" $ do
      it "orElse with partial at defined case otherwise total func" $
        apply funPartTot 42 `shouldBe` Just 42

      it "orElse with partial at undefined case otherwise total func" $
        apply funPartTot 13 `shouldBe` Just (13 + 2)

      it "orElse with two total functions should use 1st" $
        apply funTotTot' 42 `shouldBe` Just (42 + 2)


  describe "Control.Category.id" $
    it "returns original value" $
      apply Control.Category.id 42 `shouldBe` Just 42


  describe "Control.Category.." $ do
    let composition = total testTotalFunc Control.Category.. total testTotalFunc' in
      it "make composition of two total functions" $
        apply composition 42 `shouldBe` Just (42 + 2 + 42)

    let composition = partial testPartFunc' Control.Category.. partial testPartFunc' in
      it "make composition of two partial functions" $
         apply composition 42 `shouldBe` Just (42 - 10 - 10)

    let composition = total testTotalFunc Control.Category.. partial testPartFunc' in
      it "make composition of partial and total functions" $
         apply composition 42 `shouldBe` Just (42 + 2 - 10)

    let composition = partial testPartFunc' Control.Category.. total testTotalFunc in
      it "make composition of total and partial functions" $
         apply composition 42 `shouldBe` Just (42 - 10 + 2)
