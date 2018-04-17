module Block2.Bin (bin) where

-- Задание 1: Бинарные последовательности
bin :: Integer -> [[Integer]]
bin x
    | x < 0     = fail "illegal argument"
    | x == 0    = [[]]
    | otherwise = bin (x - 1) >>= \set -> [0 : set, 1 : set]
