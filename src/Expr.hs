module Expr where

import           Data.Char   (isDigit, ord)
import           Data.String

type SyntaxError = String

data Expr = Val Int
          | Plus Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Minus Expr
          | Abs Expr
          | Var String
  deriving (Eq, Show)

instance IsString Expr where
  fromString = Var

instance Num Expr where
  (+) = Plus

  fromInteger i = Val (fromInteger i)

  (-)    = Sub
  (*)    = Mul
  negate = Minus
  abs    = Abs
  signum = undefined

normalise :: Expr -> Expr
normalise expr@(Plus (Val x) (Val y))
  | x > y     = (Plus (Val y) (Val x))
  | otherwise = expr
normalise expr@(Mul (Val x) (Val y))
  | x > y     = (Mul (Val y) (Val x))
  | otherwise = expr
normalise (Plus e1 e2)  = Plus (normalise e1) (normalise e2)
normalise (Mul e1 e2)  = Mul (normalise e1) (normalise e2)
normalise e = e

simplify :: Expr -> Expr
simplify expr@(Plus (Mul x y) (Mul x'  y'))
  | x == x' = Mul x (Plus y y')
  | y == y' = Mul y (Plus x x')
  | otherwise =  expr
simplify e = e

-- 1/ I/O
repl :: IO ()
repl = getLine >>= interpret
  where
    interpret "exit" = return ()
    interpret s      = putStrLn (arithmetic s) >> repl

arithmetic :: String -> String
arithmetic s =
  case exprParser s of
    Left er  -> er
    Right ex -> show $ evalExpr ex

-- 3/ Evaluateur
evalExpr :: Expr -> Int
evalExpr (Plus v1 v2) = evalExpr v1 + evalExpr v2
evalExpr (Mul  v1 v2) = evalExpr v1 * evalExpr v2
evalExpr (Val v1)     = v1

-- 2/ Parseur
exprParser :: String -> Either SyntaxError Expr
exprParser input =
  case arithParser input of
    Left err -> Left err
    Right v  -> Right $ fst v

arithParser :: String -> Either SyntaxError (Expr, String)
arithParser cs =
  do (v1, cs')    <- intParser  cs
     case cs' of
       [] -> return (v1, [])
       _  -> do
         (plus, cs'') <- plusParser cs'
         (v2, cs''')  <- arithParser cs''
         return (plus v1 v2, cs''')


intParser :: String -> Either SyntaxError (Expr, String)
intParser (c:rest)
  | isDigit c = intParser' (ord c - ord '0') rest
  | otherwise = Left ("syntax error in '" ++ (c:rest) ++ "', expected Expr digit")
  where
    intParser' :: Int -> String -> Either SyntaxError (Expr, String)
    intParser' n (c:cs)
      | isDigit c = intParser' (n* 10 + (ord c - ord '0')) cs
      | otherwise = Right (Val n, c:cs)
    intParser' n [] = Right (Val n, [])
intParser []  = Left ("syntax error in '" ++ [] ++ "', expected a digit")

plusParser :: String -> Either SyntaxError (Expr -> Expr -> Expr, String)
plusParser ('+':rest) = Right (Plus, rest)
plusParser s          = Left ("syntax error in '" ++ s ++ "', expected a +")
