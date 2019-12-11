module Part1Spec (main, spec) where

import qualified Data.Map.Strict as M
import           Part1
import           Test.Hspec
import           Control.Monad.Reader(runReaderT)



main :: IO ()
main = hspec spec

spec :: Spec
spec =
  -- Задание 1: Арифметические выражения
  describe "eval'" $ do
    it "eval' Val" $
      runReaderT (eval' (Val 2)) M.empty `shouldBe` Right 2

    it "eval' Sum" $
      runReaderT (eval' (Sum (Val 3) (Val 2))) M.empty  `shouldBe` Right (3 + 2)

    it "eval' Sub" $
      runReaderT (eval' (Sub (Val 3) (Val 2))) M.empty  `shouldBe` Right (3 - 2)

    it "eval' Mul" $
      runReaderT (eval' (Mul (Val 3) (Val 2))) M.empty  `shouldBe` Right (3 * 2)

    it "eval' Div" $
      runReaderT (eval' (Div (Val 3) (Val 2))) M.empty  `shouldBe` Right (3 `div` 2)

    it "eval' Div with division by zero" $
      runReaderT (eval' (Div (Val 3) (Val 0))) M.empty  `shouldBe` Left DivideByZero

    it "eval' Var if var is in scope" $
      runReaderT (eval' (Var "x")) (M.singleton "x" 42) `shouldBe` Right 42

    it "eval' Var if var is not in scope" $
      runReaderT (eval' (Var "y")) (M.singleton "x" 42) `shouldBe` Left VariableNotInScope

    it "eval' Let if var is in scope" $
      runReaderT (eval' (Let "x" (Val 2) (Var "x"))) M.empty `shouldBe` Right 2

    it "eval' Let if var is not in scope" $
      runReaderT (eval' (Let "y" (Val 2) (Var "x"))) M.empty `shouldBe` Left VariableNotInScope

    it "eval' (x + 3 * (let x = 2 in x))" $
      runReaderT (eval' (Var "x" `Sum` (Val 3 `Mul` ("x" `Let` Val 2 $ Var "x")))) (M.singleton "x" 1) `shouldBe` Right 7