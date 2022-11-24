{-# LANGUAGE GADTs #-}

module Pretty (pretty) where

import Expr

pretty :: Expr -> String
pretty = expr 0
  where
  expr :: Int -> Expr -> String
  expr k (Lam b) = "λ " ++ vars !! k ++ ", " ++ expr (k + 1) b
  expr k (Let e1 e2) =
    "let " ++ vars !! k ++ " = " ++ expr k e1 ++ " in " ++ expr (k + 1) e2
  expr k e = application k e

  application :: Int -> Expr -> String
  application k (App f a) = application k f ++ " " ++ simple k a
  application k e = simple k e

  simple :: Int -> Expr -> String
  simple k (Var x) = vars !! (k - x - 1)
  simple k e = "(" ++ expr k e ++ ")"

  vars :: [String]
  vars = map return ['a'..'z'] ++ [x : show i | i <- [(1 :: Int)..], x <- ['a'..'z']]
