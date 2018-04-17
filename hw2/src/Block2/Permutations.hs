module Block2.Permutations (permutations) where

-- Задание 3: Перестановки
permutations          :: [a] -> [[a]]
permutations xs@(_:_) = [0 .. (length xs - 1)] >>= \i ->
                        permutations (deleteAt i xs) >>= \set ->
                        [xs !! i : set] where
                            deleteAt n hs = ys ++ zs where (ys, _:zs) = splitAt n hs
permutations x        = return x
