module Part4 (Action(..), actionParser) where

import           Part1
import           Part2
import           Text.Megaparsec      (eof, many, (<|>))


createVarParser :: Parser Action
createVarParser = CreateVar <$> (symbolSpace1 "mut" *> identifier) <* symbol "=" <*> exprParser

modifyVarParser :: Parser Action
modifyVarParser = ModifyVar <$> identifier <* symbol "=" <*> exprParser

printExprParser :: Parser Action
printExprParser = PrintExpr <$> (symbol "<" *> exprParser)

readVarParser :: Parser Action
readVarParser = ReadVar <$> (symbol ">" *> identifier)

forParser :: Parser Action
forParser = For <$> (symbolSpace1 "for" *> identifier) <* symbol "in" <*> exprParser <* symbol ".." <*> exprParser
                <* symbol "{" <*> many (innerActionParser <* symbol ";") <* symbol "}"

innerActionParser :: Parser Action
innerActionParser = forParser <|> createVarParser <|> modifyVarParser <|> printExprParser <|> readVarParser

actionParser :: Parser Action
actionParser = innerActionParser <* eof