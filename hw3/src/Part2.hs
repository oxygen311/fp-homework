module Part2(Parser, symbol, exprParser, symbolSpace1, identifier) where

import           Data.Void                  (Void)
import           Part1
import           Text.Megaparsec            (Parsec, between, many, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space,
                                             space1)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: String -> Parser String
symbol = L.symbol space

symbolSpace1 :: String -> Parser String
symbolSpace1 = L.symbol space1

integer :: Parser Integer
integer   = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed space integer

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser Expr
term = parens exprParser
  <|> Var <$> identifier
  <|> Val <$> signedInteger

letExpr :: Parser Expr
letExpr = Let <$> (symbolSpace1 "let" *> identifier) <* symbol "=" <*> exprParser <* symbol "in" <*> exprParser

exprParser :: Parser Expr
exprParser = letExpr <|> makeExprParser term operators

operators :: [[Operator (Parsec Void String) Expr]]
operators = [ [ InfixL (Mul <$ symbol "*")
              , InfixL (Div <$ symbol "/") ]
            , [ InfixL (Sum <$ symbol "+")
              , InfixL (Sub <$ symbol "-") ]]
