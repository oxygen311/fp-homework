module Block4.LetSpec (main, spec) where

import           Block4.Basic
import           Block4.Hard
import           Block4.Let
import           Data.Char       (isUpper)
import qualified Data.Map.Strict as Map
import           Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showAtoms" $
    it "show atoms delim by +" $
      showAtoms [N 123, I "x", N 321] `shouldBe` "123 + x + 321"

  describe "show LetExpr" $
    it "show LetExpr with atoms delim by +" $
      show (LetExpr "y" [N 123, I "x", N 321]) `shouldBe` "let y = 123 + x + 321"

  describe "string" $ do
    it "parse sequence of chars" $
      runParser (string "abc") "abcdef" `shouldBe` Right ("def", "abc")

    it "fails if even one element fail" $
      runParser (string "abz") "abcdef" `shouldBe` Left ParseError

  describe "eof" $ do
    it "ok if parse input is empty" $
      runParser eof "" `shouldBe` Right ("", ())

    it "notok if parse input is not empty" $
      runParser eof "abcdef" `shouldBe` Left ParseError

  describe "parseVar" $ do
    it "parse var in 'let x = 1 + 2 + 5" $
      runParser parseVar "let x = 1 + 2 + 5" `shouldBe` Right ("= 1 + 2 + 5", "x")

    it "parse var in 'let   y   =   x+x" $
      runParser parseVar "let   y   =   x+x" `shouldBe` Right ("=   x+x", "y")

    it "fail in 'letx=y'" $
      runParser parseVar "letx=y" `shouldBe` Left ParseError

  describe "parseAtoms" $ do
    it "parse var in '1 + 2 + 5" $
      runParser parseAtoms " 1 + 2 + 5" `shouldBe` Right ("", [N 1, N 2, N 5])

    it "parse var in '   x+x" $
      runParser parseAtoms "   x+x" `shouldBe` Right ("", [I "x", I "x"])

    it "fail in '?!?'" $
      runParser parseAtoms "?!?" `shouldBe` Left ParseError

  describe "parseLetExpr" $ do
    it "parse var in 'let x = 1 + 2 + 5" $
      runParser parseLetExpr "let x = 1 + 2 + 5" `shouldBe` Right ("",LetExpr "x" [N 1,N 2,N 5])

    it "parse var in 'let   y   =   x+x" $
      runParser parseLetExpr "let   y   =   x+x" `shouldBe` Right ("", LetExpr "y" [I "x", I "x"])

    it "fail in 'letx=y'" $
      runParser parseLetExpr "letx=y" `shouldBe` Left ParseError

  describe "evalAtoms" $ do
    it "eval Integer by summ them" $
      evalAtoms Map.empty [N 1, N 2, N 3] `shouldBe` Just (1 + 2 + 3)

    it "work with variables" $
      evalAtoms (Map.singleton "x" 42) [I "x", I "x", N 3] `shouldBe` Just (42 + 42 + 3)

    it "fail if variable is not in map" $
      evalAtoms (Map.singleton "x" 42) [I "x", I "y", N 3] `shouldBe` Nothing

  describe "simplify" $ do
    it "simplify if ther is no vars after '='" $
      simplify [LetExpr "x" [N 1, N 2],
                LetExpr "y" [N 3, N 4],
                LetExpr "z" [N 5, N 6]]
                `shouldBe`
                [Just (LetExpr "x" [N (1 + 2)]),
                 Just (LetExpr "y" [N (3 + 4)]),
                 Just (LetExpr "z" [N (5 + 6)])]

    it "can use previos var" $
      simplify [LetExpr "x" [N 1],
                LetExpr "y" [I "x", N 2],
                LetExpr "z" [I "x", I "y", N 3]]
                `shouldBe`
                [Just (LetExpr "x" [N 1]),
                 Just (LetExpr "y" [N (1 + 2)]),
                 Just (LetExpr "z" [N (1 + 1 + 2 + 3)])]

    it "variable can rewrite itself" $
      simplify [LetExpr "x" [N 1],
                LetExpr "y" [I "x"],
                LetExpr "x" [I "x", N 42],
                LetExpr "y" [I "x"]]
                `shouldBe`
                [Just (LetExpr "x" [N 1]),
                 Just (LetExpr "y" [N 1]),
                 Just (LetExpr "x" [N (42 + 1)]),
                 Just (LetExpr "y" [N (42 + 1)])]

    it "fail if variable is not defined" $
      simplify [LetExpr "x" [I "z"]] `shouldBe` [Nothing]

  describe "parseAndSimplify" $ do
    it "parse and simplify if ther is no vars after '='" $
      parseAndSimplify ["let x = 1 + 2", "let y = 3 + 4", "let z = 5 + 6"]
      `shouldBe`       ["let x = 3"    , "let y = 7"    , "let z = 11"   ]

    it "can use previos var" $
       parseAndSimplify ["let x = 1", "let y = x + 2", "let z = x + y + 3"]
       `shouldBe`       ["let x = 1", "let y = 3"    , "let z = 7"        ]

    it "variable can rewrite itself" $
       parseAndSimplify ["let x = 1", "let y = x", "let x = x + 42", "let y = x"]
       `shouldBe`       ["let x = 1", "let y = 1", "let x = 43"    , "let y = 43"]

    it "fail if variable is not defined (return empty list)" $
       parseAndSimplify ["let x = z"]
       `shouldBe`       []

    it "don't simplify if variable is not defined" $
       parseAndSimplify ["let x = 1", "let y = z", "let x = x + 42", "let y = q"]
       `shouldBe`       ["let x = 1",              "let x = 43"                 ]

    it "don't parse illegal statements" $
       parseAndSimplify ["let x = 5c", "let x = x y z", "wut", "is", "it"]
       `shouldBe`       []

