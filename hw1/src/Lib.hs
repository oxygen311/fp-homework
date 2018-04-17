module Lib (order3, highestBit, smartReplicate, contains,
            removeAt, collectEvery, stringSum, mergeSort,
            nextDay, afterDays, isWeekend, daysToParty,
            health, attack, fType, fight,
            vectorLength, zip2Vectors, vectorSum, scalarMul, vectorSub, vectorMul,
            toInteger', even', div', mod', gcd',
            isEmpty, size, exists, fromList, toList, splitOn, joinWith,
            maybeConcat, eitherConcat, runIdentity, getEndo, getArrow) where

import           Data.Char      (isDigit)
import           Data.List      (sort)
import           Data.Monoid    ()
import           Data.Semigroup (Semigroup (..))
import           Data.String    (String)
import           Data.Maybe     (catMaybes)
import           Data.Either    (lefts, rights)

-- Block 1: Simple functions
-- Задание 1
order3           :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = (x', y', z') where [x', y', z'] = sort [x, y, z]

-- Задавние 2 (Advanced)
highestBit   :: (Num a, Num b, Ord b) => b -> (b, a)
highestBit x = rec' 0 1 where
      rec' i acc
            | (acc * 2) > x = (acc, i)
            | otherwise     = rec' (i + 1) (acc * 2)

-- Задание 3
smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x -> replicate x x)

-- Задание 4
contains   :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

-- Block 2: Pattern matching

-- Задание 1 (Advanced)
removeAt          :: Integer -> [a] -> (Maybe a, [a])

removeAt n (x:xs)
    | n < 0     = (Nothing, x : xs)
    | n == 0    = (Just x, xs)
    | otherwise = (mb, x : ys)
            where (mb, ys) = removeAt (n - 1) xs
removeAt _ _      = (Nothing, [])

-- Задание 2
collectEvery      :: Int -> [a] -> ([a], [a])
collectEvery k xs = rec' k xs where
    rec' 1 (x:xs') = (ys, x : zs)
        where (ys, zs) = rec' k xs'

    rec' n (x:xs') = (x : ys, zs)
        where (ys, zs) = rec' (n - 1) xs'

    rec' _ _      = ([], [])

-- Задание 3 (Advanced)
stringSum :: String -> Integer
stringSum = sum . map (read . deleteUnaryPlus) . words where
    deleteUnaryPlus ('+':x:xs)
        | isDigit x = x : xs
        | otherwise = ""
    deleteUnaryPlus x = x

-- Задание 4
mergeSort :: Ord a => [a] -> [a]
mergeSort a
    | length a > 1  = mergeLists (mergeSort p1) (mergeSort p2)
    | length a == 1 = a
    | otherwise     = []
    where (p1, p2) = splitAt (length a `div` 2) a
          mergeLists :: Ord a => [a] -> [a] -> [a]
          mergeLists (x:xs) (y:ys)
             | x <= y    = x : mergeLists xs     (y:ys)
             | otherwise = y : mergeLists (x:xs) ys
          mergeLists [] ys = ys
          mergeLists xs [] = xs

-- Block 3: ATDs
-- Задание 1: Дни недели
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Ord, Eq, Enum, Bounded, Show)

nextDay        :: Day -> Day
nextDay Sunday = Monday
nextDay x      = succ x

prevDay        :: Day -> Day
prevDay Monday = Sunday
prevDay x      = pred x

afterDays       :: Day -> Int -> Day
afterDays day n
    | n > 0     = afterDays (nextDay day) (n `rem` 7 - 1)
    | n < 0     = afterDays (prevDay day) (n `rem` 7 + 1)
    | otherwise = day

isWeekend     :: Day -> Bool
isWeekend day = day > Friday

daysToParty        :: Day -> Int
daysToParty Friday = 0
daysToParty day    = daysToParty (nextDay day) + 1

-- Задание 2: Монстры и рыцари (Advanced)
data Fighter = Fighter { health :: Integer, attack :: Integer, fType :: FighterType}
    deriving (Show, Read)

data FighterType = Knight | Monster deriving (Show, Read)

fight :: Fighter -> Fighter -> (Fighter, Integer)
fight = rec' 0 where
    rec' acc (Fighter h a t) (Fighter h' a' t')
        | h < 0     = (Fighter h' a' t', acc)
        | otherwise = rec' (acc + 1) (Fighter (h' - a) a' t') (Fighter h a t)

-- Задание 3: Векторы
data Vector a = Vector2D a a | Vector3D a a a
    deriving (Eq, Show, Read)

vectorLength                  :: Floating a => Vector a -> a
vectorLength (Vector2D x y)   = sqrt (x * x + y * y)
vectorLength (Vector3D x y z) = sqrt (x * x + y * y + z * z)

zip2Vectors                                        :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
zip2Vectors f (Vector3D x y z) (Vector3D x' y' z') = Vector3D (f x x') (f y y') (f z z')
zip2Vectors f (Vector3D x y z) (Vector2D x' y')    = Vector3D (f x x') (f y y') z
zip2Vectors f (Vector2D x y)   (Vector3D x' y' z') = Vector3D (f x x') (f y y') z'
zip2Vectors f (Vector2D x y)   (Vector2D x' y')    = Vector2D (f x x') (f y y')

vectorSum :: Num a => Vector a -> Vector a -> Vector a
vectorSum = zip2Vectors (+)

scalarMul                                      :: Num a => Vector a -> Vector a -> a
scalarMul (Vector3D x y z) (Vector3D x' y' z') = x * x' + y * y' + z * z'
scalarMul (Vector3D x y z) (Vector2D x' y')    = x * x' + y * y' + z
scalarMul (Vector2D x y)   (Vector3D x' y' z') = x * x' + y * y' + z'
scalarMul (Vector2D x y)   (Vector2D x' y')    = x * x' + y * y'

vectorSub :: Floating a => Vector a -> Vector a -> Vector a
vectorSub = zip2Vectors (\x y -> x * x - y * y)

vectorMul                                      :: Num a => Vector a -> Vector a -> Vector a
vectorMul (Vector3D x y z) (Vector3D x' y' z') = Vector3D (y * z' - y' * z) (-(x * z' - x' * z)) (x * y' - x' * y)
vectorMul (Vector3D x y z) (Vector2D x' y')    = vectorMul (Vector3D x y z) (Vector3D x' y' 0)
vectorMul (Vector2D x y)   (Vector3D x' y' z') = vectorMul (Vector3D x y 0) (Vector3D x' y' z')
vectorMul (Vector2D x y)   (Vector2D x' y')    = vectorMul (Vector3D x y 0) (Vector3D x' y' 0)

-- Задание 4: Натуральные числа
data Nat = Z | S Nat
    deriving (Show, Read)

instance Num Nat where
    a + Z     = a
    a + (S c) = S $ a + c

    _ * Z     = Z
    a * (S c) = a * c + a

    a     - Z     = a
    (S c) - (S d) = c - d
    _     - _     = error "Subtrahend is bigger then minuend"

    fromInteger 0 = Z
    fromInteger n
        | n < 0     = error "Nat is positive"
        | otherwise = S $ fromInteger (n - 1)

    abs = id

    signum Z = Z
    signum _ = S Z

toInteger' :: Nat -> Integer
toInteger' a = rec' a 0 where
    rec' Z     acc = acc
    rec' (S b) acc = rec' b (acc + 1)

instance Eq Nat where
    Z     == Z     = True
    (S a) == (S b) = a == b
    _     == _     = False

instance Ord Nat where
    Z     <= _     = True
    (S a) <= (S b) = a <= b
    _     <= _     = False

-- Advanced
even'       :: Nat -> Bool
even' (S x) = not $ even' x
even' Z     = True

div' :: Nat -> Nat -> Nat
div' a b
    | b == 0    = error "Division by zero"
    | a >= b    = S $ div' (a - b) b
    | otherwise = Z

mod' :: Nat -> Nat -> Nat
mod' a b
    | b == 0    = error "Division by zero"
    | a >= b    = mod' (a - b) b
    | otherwise = a

gcd'     :: Nat -> Nat -> Nat
gcd' 0 0 = error "GCD is not defined"
gcd' a 0 = a
gcd' a b = gcd' b (mod' a b)

-- Задание 5: Растительность
data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Read)

isEmpty      :: Tree a -> Bool
isEmpty Leaf = False
isEmpty _    = True

size              :: Tree a -> Integer
size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

exists        :: Ord a => a -> Tree a -> Bool
exists _ Leaf = False
exists n (Node x l r)
    | x > n     = exists n l
    | x < n     = exists n r
    | otherwise = True

insert'        :: Ord a => a -> Tree a -> Tree a
insert' n Leaf = Node n Leaf Leaf
insert' n (Node x l r)
    | x > n     = Node x (insert' n l) r
    | x < n     = Node x l (insert' n r)
    | otherwise = Node x l r

fromList    :: Ord a => [a] -> Tree a
fromList [] = Leaf
fromList xs = foldr insert' Leaf xs

-- Блок 4: Сворачиваемся
-- Задание 1: Деревянный Foldable
instance Foldable Tree where
    foldMap _ Leaf         = mempty
    foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

    foldr _ z Leaf         = z
    foldr f z (Node x l r) = foldr f (f x (foldr f z r)) l

toList :: Tree a -> [a]
toList = foldr (:) []

-- Задание 2: Разбиваемся
splitOn    :: Eq a => a -> [a] -> [[a]]
splitOn ch = foldr fun [[]] where
    fun c (x:xs)
        | c == ch   = []:x:xs
        | otherwise = (c:x):xs
    fun _ [] = [[]]

-- Advanced
joinWith       :: Eq a => a -> [[a]] -> [a]
joinWith _ []  = []
joinWith ch xs = foldr1 fun xs where
    fun ys z  = ys ++ ch : z

-- Блок 5: Моноиды
-- Задание 1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = concat . catMaybes

-- Advanced
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat xs = (mconcat (lefts xs), mconcat (rights xs))

-- Задание 2
data NonEmpty a = a :| [a]
    deriving (Show, Read)

instance Semigroup (NonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mappend (Identity a) (Identity b) = Identity (mappend a b)
    mempty = Identity mempty

-- Advanced
-- 1) Name
newtype Name = Name String
    deriving (Show, Read)

instance Semigroup Name where
    an@(Name a) <> bn@(Name b)
        | a == ""   = bn
        | b == ""   = an
        | otherwise = Name (a ++ '.' : b)

instance Monoid Name where
    mappend = (<>)
    mempty  = Name  ""

-- 2) Endo
newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup (Endo a) where
    Endo f <> Endo g = Endo (f . g)
--     (Endo f <> Endo g) <> Endo h = Endo (f . g) <> Endo h = Endo (f . g . h)

instance Monoid (Endo a) where
    mempty  = Endo id
    mappend = (<>)

-- 3) Arrow
newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Semigroup b => Semigroup (Arrow a b) where
    Arrow f <> Arrow g        = Arrow (f <> g)

instance Monoid    b => Monoid    (Arrow a b) where
    Arrow f `mappend` Arrow g = Arrow (f `mappend` g)
    mempty                    = Arrow mempty

-- Задание 3
instance Ord a => Semigroup (Tree a) where
    t <> (Node x l r) = insert' x t <> l <> r
    t <> Leaf         = t

instance Ord a => Monoid (Tree a) where
    mappend = (<>)
    mempty = Leaf