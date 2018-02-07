module ExprParserSpec where

import           Data.Char       (isDigit, ord)
import           Test.Hspec
import           Test.QuickCheck

type SyntaxError = String

data Expr = Val Int
          | Plus Expr Expr
  deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (Plus v1 v2) = evalExpr v1 + evalExpr v2
evalExpr (Val v1)     = v1

exprParser :: String -> Either SyntaxError Expr
exprParser [c1]          = intParser [c1]
exprParser (c1:c2:c3:cs) =
  do v1   <- intParser  [c1]
     plus <- plusParser [c2]
     v2   <- exprParser (c3:cs)
     return $ plus v1 v2
exprParser _             = Left "syntax error"


intParser :: String -> Either SyntaxError Expr
intParser (c:[])
  | isDigit c = Right (Val $ ord c - ord '0')
  | otherwise = Left "syntax error"

plusParser :: String -> Either SyntaxError (Expr -> Expr -> Expr)
plusParser ('+':[]) = Right Plus
plusParser _        = Left "syntax error"


spec :: Spec
spec = describe "Analyseur syntaxique d'additions à 1 chiffre" $ do

  describe "analyseur de plus" $ do
    it "parse '1+2'" $ do
      exprParser "1+2" `shouldBe` Right (Val 1 `Plus` Val 2)

    it "parse '2+1'" $ do
      exprParser "2+1" `shouldBe` Right (Val 2 `Plus` Val 1)

    it "parse 'a+1' est une erreur" $ do
      exprParser "a+1" `shouldBe` Left "syntax error"

    it "parse '2-1' est une erreur" $ do
      exprParser "2-1" `shouldBe` Left "syntax error"

    it "parse '2+a' est une erreur" $ do
      exprParser "2+a" `shouldBe` Left "syntax error"

    it "parse '2+1+3'" $ do
      exprParser "2+1+3" `shouldBe` Right (Val 2 `Plus` (Val 1 `Plus` Val 3))

    it "parse '2+1+' est une erreur" $ do
      exprParser "2+1+" `shouldBe` Left "syntax error"

    it "parse '+' est une erreur" $ do
      exprParser "+" `shouldBe` Left "syntax error"

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

  describe "Evaluateur arithmétique" $ do

    it "calcule '1+2' retourne 3" $
      evalExpr (Plus (Val 1) (Val 2)) `shouldBe` 3

    it "calcule '1+3' retourne 4" $
      evalExpr (Plus (Val 1) (Val 3)) `shouldBe` 4

    it "calcule '1+3+5' retourne 9" $
      evalExpr (Plus (Val 1) (Plus (Val 3) (Val 5))) `shouldBe` 9

    it "calcule '(1+3)+5' retourne 9" $
      evalExpr (Plus (Plus (Val 1) (Val 3)) (Val 5)) `shouldBe` 9

newtype Digit = Digit Char
  deriving (Eq, Show)

instance Arbitrary Digit where
  arbitrary = Digit <$> elements ['0'.. '9']

analyseSingleDigit :: Digit -> Bool
analyseSingleDigit (Digit c) =
  intParser [c] == Right (Val $ read [c])
