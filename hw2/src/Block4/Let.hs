module Block4.Let(Var(..), parseVar, LetExpr(..),
        parseAtoms, parseLetExpr, string, showAtoms,
        eof, evalAtoms, simplify, parseAndSimplify) where

import           Block4.Basic
import           Block4.Hard
import           Control.Monad   (liftM, liftM2, (>=>))
import           Data.Char       (isSpace)
import           Data.Either     (rights)
import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)


instance Monad (Monstupar c) where
    return a = Monstupar $ \c -> Right (c , a)
    p >>= f  = Monstupar $ runParser p >=> (\(cs, a) -> runParser (f a) cs)

type Var = Ident

data LetExpr = LetExpr Var [Atom]
   deriving (Eq)

showAtoms :: [Atom] -> String
showAtoms = intercalate " + " . map show

instance Show LetExpr where
    show (LetExpr v atoms) = "let " ++ v ++ " = " ++ showAtoms atoms

string :: Eq c => [c] -> Monstupar c [c]
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

eof :: Monstupar s ()
eof = Monstupar $ \s -> case s of
    [] -> Right (s , ())
    _  -> Left ParseError

parseVar :: Monstupar Char Var
parseVar = spaces *> string "let" *> oneOrMoreSpaces *> ident <* spaces

parseAtoms :: Monstupar Char [Atom]
parseAtoms = (:) <$> parseAtom <*> zeroOrMore (char '+' *> parseAtom)

parseLetExpr :: Monstupar Char LetExpr
parseLetExpr = LetExpr <$> parseVar <* char '=' <*> parseAtoms <* eof

evalAtoms :: Map.Map Var Integer -> [Atom] -> Maybe Integer
evalAtoms m = foldr func (Just 0) where
    func _ Nothing      = Nothing
    func (N n) (Just x) = Just (n + x)
    func (I i) x        = liftM2 (+) (Map.lookup i m) x

simplify :: [LetExpr] -> [Maybe LetExpr]
simplify = simplify' Map.empty where
    simplify' m (LetExpr v atoms: xs) = case evaledAtoms of
                                            Just val -> Just (LetExpr v [N val]) : simplify' (Map.insert v val m) xs
                                            Nothing  -> Nothing : simplify' m xs
                                            where evaledAtoms = evalAtoms m atoms
    simplify' _ _ = []

parseAndSimplify :: [String] -> [String]
parseAndSimplify xs = map show $ catMaybes $ simplify $ map snd $ rights $ map (runParser parseLetExpr) xs
