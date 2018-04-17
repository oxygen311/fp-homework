{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Block4.Basic (Monstupar(..), satisfy, char, posInt, second, abParser, abParser_, intPair, intOrUppercase,
                     (<|>), empty, pure, (<*>), fmap, ParseError(..)) where

import           Control.Monad (void, (>=>))
import           Data.Char     (isDigit, isUpper)

data ParseError = ParseError
    deriving (Eq, Show)

newtype Monstupar c a -- s - тип того, что парсим, а-ля Char
                      -- a - тип того, что вернёт парсер
    = Monstupar { runParser :: [c] -> Either ParseError ([c], a) }


satisfy   :: (c -> Bool) -> Monstupar c c
satisfy p = Monstupar f where
    f [] = Left ParseError
    f (x:xs)
        | p x = Right (xs, x)
        | otherwise = Left ParseError

char :: Eq c => c -> Monstupar c c
char = satisfy . (==)

posInt :: Monstupar Char Integer
posInt = Monstupar f
    where
        f xs
          | null ns   = Left ParseError
          | otherwise = Right (rest, read ns)
          where (ns, rest) = span isDigit xs

second :: (a -> b) -> (c, a) -> (c, b)
second f (c, a) = (c, f a)

instance Functor (Monstupar c) where
--  fmap :: forall a b. (a -> b) -> Monstupar c a -> Monstupar c b
    fmap f (Monstupar p) = Monstupar $ fmap (second f) . p

instance Applicative (Monstupar c) where
    pure                  :: a -> Monstupar c a
    pure x                = Monstupar $ \s -> Right (s, x)
--  (<*>)                 :: Monstupar c (a -> b) -> Monstupar c a -> Monstupar c b
    (Monstupar p1) <*> p2 = Monstupar $ p1 >=> (\(s, x) -> runParser (fmap x p2) s)

abParser :: Monstupar Char (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Monstupar Char ()
abParser_ = void abParser

intPair :: Monstupar Char [Integer]
intPair = toList <$> posInt <* char ' ' <*> posInt
    where toList x y = [x, y]


class Applicative f => Alternative f where
   empty :: f a
   (<|>) :: f a -> f a -> f a


instance Alternative (Either ParseError) where
    empty           = Left ParseError
    (Right a) <|> _ = Right a
    _         <|> b = b

instance Alternative (Monstupar c) where
    empty                             = Monstupar $ const $ Left ParseError
    (Monstupar p1) <|> (Monstupar p2) = Monstupar $ \s -> p1 s <|> p2 s

intOrUppercase :: Monstupar Char ()
intOrUppercase = void posInt <|> void (satisfy isUpper)

