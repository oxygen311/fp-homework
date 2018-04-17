module Block2.Combinations (combinations) where

-- Задание 2: Сочетания
combinations :: Integer -> Integer -> [[Integer]]
combinations _ 0 = []
combinations n k' = combinations' 1 k' where
     combinations' _ 0         = [[]]
     combinations' minBound' k = [minBound' .. n] >>= \i ->
                                 combinations' (i + 1) (k - 1) >>= \set ->
                                 [i:set]
