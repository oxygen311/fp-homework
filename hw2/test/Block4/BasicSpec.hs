module Block4.BasicSpec (main, spec) where

import           Block4.Basic
import           Data.Char    (isUpper)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "satisfy" $ do
    it "satisfy isUpper" $
      runParser (satisfy isUpper) "ABC" `shouldBe` Right ("BC",'A')

    it "dont satisfy isUpper" $
      runParser (satisfy isUpper) "abc" `shouldBe` Left ParseError

  describe "char" $ do
    it "char is correct" $
      runParser (char 'x') "xyz" `shouldBe` Right ("yz",'x')

    it "char is incorrect" $
      runParser (char 'x') "abc" `shouldBe` Left ParseError

  describe "posInt" $ do
    it "parse positive Integer" $
      runParser posInt "123 xyz" `shouldBe` Right (" xyz", 123)

    it "fail at not digits" $
      runParser posInt "xyz 123" `shouldBe` Left ParseError

  describe "second" $
    it "just apply function to second element of pair" $
      second (+2) (1, 2) `shouldBe` (1, 4)

  describe "Functor (Monstupar c)" $ do
    it "fmap id  ==  id" $
      runParser (fmap id posInt) "123" `shouldBe` runParser posInt "123"

    it "fmap (f . g)  ==  fmap f . fmap g" $
      runParser (fmap ((+2) . (+5)) posInt) "123" `shouldBe` runParser ((fmap (+2) . fmap (+5)) posInt) "123"

  describe "Applicative (Monstupar c)" $ do
    it "identity" $
      runParser (pure id <*> posInt) "123" `shouldBe` runParser posInt "123"

    it "homomorphism" $
      runParser (id <$> posInt) "12f" `shouldBe` runParser posInt "12f"

  describe "Alternative (Monstupar c)" $
    it "empty" $
      runParser (empty <|> posInt) "12" `shouldBe` Right ("", 12)

  describe "abParser" $ do
    it "satisfy 'a' and 'b' in head" $
      runParser abParser "abcdef" `shouldBe` Right ("cdef", ('a', 'b'))

    it "dont satisfy otherwise" $
      runParser abParser "xyz123" `shouldBe` Left ParseError

  describe "abParser_" $ do
    it "satisfy 'a' and 'b' in head and returns ()" $
      runParser abParser_ "abcdef" `shouldBe` Right ("cdef", ())

    it "dont satisfy otherwise" $
      runParser abParser_ "xyz123" `shouldBe` Left ParseError

  describe "intPair" $ do
    it "reads two integer values separated by a space and returns the integer values in a list" $
      runParser intPair "12 34" `shouldBe` Right ("", [12, 34])

    it "fail at not digits" $
      runParser intPair "xs ys" `shouldBe` Left ParseError

    it "fail if second word is not digits" $
      runParser intPair "12 ys" `shouldBe` Left ParseError

  describe "intOrUppercase" $ do
    it "found int" $
      runParser intOrUppercase "342abcd" `shouldBe` Right ("abcd", ())

    it "found isUpper" $
      runParser intOrUppercase "XYZ" `shouldBe` Right ("YZ", ())

    it "fails otherwise" $
      runParser intOrUppercase "foo" `shouldBe` Left ParseError
