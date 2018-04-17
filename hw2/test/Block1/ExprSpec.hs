module Block1.ExprSpec (main, spec) where

import           Block1.Expr
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  -- Задание 1: Арифметические выражения
  describe "eval" $ do
    it "eval Const" $
      eval (Const 2) `shouldBe` Right 2

    it "eval Sum" $
      eval (Sum (Const 3) (Const 2)) `shouldBe` Right (3 + 2)

    it "eval Sub" $
      eval (Sub (Const 3) (Const 2)) `shouldBe` Right (3 - 2)

    it "eval Mul" $
      eval (Mul (Const 3) (Const 2)) `shouldBe` Right (3 * 2)

    it "eval Div" $
      eval (Div (Const 3) (Const 2)) `shouldBe` Right (3 `div` 2)

    it "eval Div with division by zero" $
      eval (Div (Const 3) (Const 0)) `shouldBe` Left DivideByZero

    it "eval Pow" $
      eval (Pow (Const 3) (Const 2)) `shouldBe` Right (3 ^ 2)

    it "eval Pow with negative exponent" $
      eval (Pow (Const 3) (Const (-1))) `shouldBe` Left NegativeExponent
