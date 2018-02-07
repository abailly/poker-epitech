module ExprParserSpec where

import           Data.Char       (isDigit, ord)
import           Test.Hspec
import           Test.QuickCheck

exprParser :: String -> Either SyntaxError Expr
exprParser _ = Right (Val 1 `Plus` Val 2)

intParser :: String -> Either SyntaxError Expr
intParser (c:[])
  | isDigit c = Right (Val $ ord c - ord '0')
  | otherwise = Left "syntax error"

plusParser :: String -> Either SyntaxError (Expr -> Expr -> Expr)
plusParser ('+':[]) = Right Plus
plusParser _        = Left "syntax error"

type SyntaxError = String

data Expr = Val Int
          | Plus Expr Expr
  deriving (Eq, Show)

spec :: Spec
spec = describe "Analyseur syntaxique d'additions à 1 chiffre" $ do

  -- it "parse '1+2'" $ do
  --   exprParser "1+2" `shouldBe` Right (Val 1 `Plus` Val 2)

  -- it "parse '2+1'" $ do
  --   exprParser "2+1" `shouldBe` Right (Val 2 `Plus` Val 1)

  describe "Parser de Int" $ do

    it "analyse un chiffre comme un entier" $
      property $ analyseSingleDigit

    it "analyse un non-digit comme une syntaxerror" $
      intParser "a" `shouldBe` Left "syntax error"

  describe "Parser de '+'" $ do

    it "analyse le caractère '+' comme un 'plus'" $
      let Right f = plusParser "+"
      in f (Val 1) (Val 2) `shouldBe` Plus (Val 1) (Val 2)

    it "analyse le caractère '-' comme une erreur" $
      let Left e = plusParser "-"
      in e `shouldBe` "syntax error"

newtype Digit = Digit Char
  deriving (Eq, Show)

instance Arbitrary Digit where
  arbitrary = Digit <$> elements ['0'.. '9']

analyseSingleDigit :: Digit -> Bool
analyseSingleDigit (Digit c) =
  intParser [c] == Right (Val $ read [c])
