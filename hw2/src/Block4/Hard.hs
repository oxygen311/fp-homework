module Block4.Hard(zeroOrMore, oneOrMore, spaces, ident,
                   Ident, Atom(..), parseAtom, parseSExpr,
                   SExpr(..), oneOrMoreSpaces, checkIsNotAlpha) where

import           Block4.Basic
import           Data.Char    (isAlpha, isAlphaNum, isSpace)

zeroOrMore :: Monstupar c a -> Monstupar c [a]
zeroOrMore p1 = oneOrMore p1 <|> pure []

oneOrMore :: Monstupar c a -> Monstupar c [a]
oneOrMore p1 = (:) <$> p1 <*> zeroOrMore p1

spaces :: Monstupar Char String
spaces = zeroOrMore (satisfy isSpace)

oneOrMoreSpaces :: Monstupar Char String
oneOrMoreSpaces = oneOrMore (satisfy isSpace)

ident :: Monstupar Char String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident
   deriving (Eq)

instance Show Atom where
    show (N n) = show n
    show (I i) = i

data SExpr = A Atom
           | Comb [SExpr]
   deriving (Show, Eq)

checkIsNotAlpha :: Monstupar Char ()
checkIsNotAlpha = Monstupar f where
                  f [] = Right ([], ())
                  f a@(x:_)
                      | isAlpha x = Left ParseError
                      | otherwise = Right (a, ())

parseAtom :: Monstupar Char Atom
parseAtom = spaces *> (N <$> posInt <* checkIsNotAlpha) <|> (I <$> ident) <* spaces

parseSExpr :: Monstupar Char SExpr
parseSExpr = spaces *> (A <$> parseAtom) <|> (Comb <$> (char '(' *> spaces *> zeroOrMore parseSExpr <* char ')')) <* spaces
