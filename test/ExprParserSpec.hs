module ExprParserSpec where

import           Data.Char  (ord)
import           Test.Hspec

exprParser :: String -> Either SyntaxError Expr
exprParser _ = Right (Val 1 `Plus` Val 2)

intParser :: String -> Either SyntaxError Expr
intParser (c:[])
  | c `elem` [ '0' .. '9'] = Right (Val $ ord c - ord '0')

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

    it "parse '1'" $ do
      intParser "1" `shouldBe` Right (Val 1)

    it "parse '2'" $ do
      intParser "2" `shouldBe` Right (Val 2)
