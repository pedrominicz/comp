module Parse (parse) where

import Expr

import Control.Monad.Reader
import Data.List
import Text.Parsec hiding (parse)

type Parser = ParsecT String () (Reader [String])

parse :: String -> Maybe Expr
parse str =
  case runReader (runParserT (spaces *> expr <* eof) () "" str) [] of
    Left _ -> Nothing
    Right e -> Just e

expr :: Parser Expr
expr = lam <|> app

lam :: Parser Expr
lam = do
  symbol '\\'
  -- TODO: multiple arguments.
  x <- name
  symbol ','
  b <- local (x :) expr
  return (Lam b)

app :: Parser Expr
app = (parens expr <|> var <|> num) `chainl1` return App

var :: Parser Expr
var = do
  x <- name
  env <- ask
  case elemIndex x env of
    Just x -> return (Var x)
    Nothing -> mzero

num :: Parser Expr
num = do
  n <- read <$> many1 digit
  spaces
  return $ Lam (Lam (foldr App (Var 0) (replicate n (Var 1))))

symbol :: Char -> Parser ()
symbol c = char c *> spaces

name :: Parser String
name = do
  c <- letter
  cs <- many alphaNum
  spaces
  return (c:cs)

parens :: Parser a -> Parser a
parens = between (symbol '(') (symbol ')')
