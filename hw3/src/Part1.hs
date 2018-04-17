module Part1
    () where

-- import           Control.Monad (liftM2)

-- Block 1: Optional Monad
-- Задание 1: Арифметические выражения
data Expr = Const Int | Var String | Sum Expr Expr | Sub Expr Expr | Mul Expr Expr | Let Expr Expr Expr
    deriving (Read, Show, Eq)

data ArithmeticError = DivideByZero | NegativeExponent
    deriving (Read, Show, Eq)

-- eval :: Part1 -> Either ArithmeticError Int
-- eval (Const x) = return x
-- eval (Sum l r) = liftM2 (+) (eval l) (eval r)
-- eval (Sub l r) = liftM2 (-) (eval l) (eval r)
-- eval (Mul l r) = liftM2 (*) (eval l) (eval r)
-- eval (Div l r) = eval l >>= \x ->
--                  eval r >>= \y ->
--                  evalDiv x y
-- eval (Pow l r) = eval l >>= \x ->
--                  eval r >>= \y ->
--                  evalPow x y
--
-- evalDiv     :: Int -> Int -> Either ArithmeticError Int
-- evalDiv x y
--     | y == 0    = Left  DivideByZero
--     | otherwise = Right $ div x y
--
-- evalPow     :: Int -> Int -> Either ArithmeticError Int
-- evalPow x y
--     | y < 0     = Left  NegativeExponent
--     | otherwise = Right $ x ^ y
