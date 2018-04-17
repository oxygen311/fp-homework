module Block4.HardSpec (main, spec) where

import           Block4.Basic
import           Block4.Hard
import           Data.Char    (isUpper)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "zeroOrMore" $ do
    it "satisfy all Upper symbols" $
      runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Right ("dEfgH", "ABC")

    it "otherwise succeeds and returns the empty list" $
      runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Right ("abcdeFGh", "")

  describe "oneOrMore" $ do
    it "satisfy all Upper symbols" $
      runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe` Right ("dEfgH", "ABC")

    it "otherwise fail" $
      runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Left ParseError

  describe "spaces" $ do
    it "parse all spaces in begin" $
      runParser spaces "         \n     abc" `shouldBe` Right ("abc", "         \n     ")

    it "otherwise succeeds and returns the empty list" $
      runParser spaces "abc" `shouldBe` Right ("abc", "")

  describe "ident" $ do
    it "parse identifier, ends with space" $
      runParser ident "foobar baz" `shouldBe` Right (" baz", "foobar")

    it "parse all string as identifier" $
      runParser ident "foo33fA" `shouldBe` Right ("", "foo33fA")

    it "fails if first char is Digit" $
      runParser ident "2bad" `shouldBe` Left ParseError

    it "fails on empty list" $
      runParser ident "" `shouldBe` Left ParseError

  describe "checkIsNotAlpha" $ do
    it "check is not aplha and parse nothing on alpha" $
     runParser checkIsNotAlpha "abc" `shouldBe` Left ParseError

    it "check is not aplha and parse nothing on digit" $
     runParser checkIsNotAlpha "123" `shouldBe` Right ("123", ())

  describe "parseAtom" $ do
    it "parse integer value" $
      runParser parseAtom "123" `shouldBe` Right ("", N 123)

    it "parse identifier" $
      runParser parseAtom "x" `shouldBe` Right ("", I "x")

    it "fails on other chars" $
      runParser parseAtom "!?!" `shouldBe` Left ParseError

    it "fails empty list" $
      runParser parseAtom "" `shouldBe` Left ParseError

  describe "parseSExpr" $ do
    it "parse SExpt" $
      runParser parseSExpr " (bar (foo) 3 5 874)" `shouldBe` Right ("",Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)])

    it "trim spaces" $
      runParser parseSExpr "   5   " `shouldBe` Right ("",A (N 5))

    it "handle a lot of spaces" $
      runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )"
        `shouldBe` Right ("",Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]])

    it "fail if brackets is wrong" $
      runParser parseSExpr "   (5   " `shouldBe` Left ParseError

    it "fail if not ident or int" $
      runParser parseSExpr " ! ? ! " `shouldBe` Left ParseError

