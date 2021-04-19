module Main where

import Gen
import Name
import Typing
import KNormal
import qualified Lexer as L
import qualified Parser as P

import Control.Monad.Trans
import Data.Maybe
import qualified Data.Set as S

cmp :: KNormal -> KNormal -> Bool
cmp e1 e2 = case (e1, e2) of
  (Unit, Unit) -> True
  (Int x, Int y) | x == y -> True
  (Float x, Float y) | x == y -> True
  (Neg _, Neg _) -> True
  (Add _ _, Add _ _) -> True
  (Sub _ _, Sub _ _) -> True
  (FNeg _, FNeg _) -> True
  (FAdd _ _, FAdd _ _) -> True
  (FSub _ _, FSub _ _) -> True
  (FMul _ _, FMul _ _) -> True
  (FDiv _ _, FDiv _ _) -> True
  (IfEq _ _ e11 e12, IfEq _ _ e21 e22) -> cmp e11 e21 && cmp e12 e22
  (IfLE _ _ e11 e12, IfLE _ _ e21 e22) -> cmp e11 e21 && cmp e12 e22
  (Let _ e11 e12, Let _ e21 e22) -> cmp e11 e21 && cmp e12 e22
  (Var _, Var _) -> True
  (LetRec _ xs e11 e12, LetRec _ ys e21 e22) -> length xs == length ys && cmp e11 e21 && cmp e12 e22
  (App _ xs, App _ ys) -> length xs == length ys
  (Tuple xs, Tuple ys) -> length xs == length ys
  (LetTuple xs _ e1, LetTuple ys _ e2) -> length xs == length ys && cmp e1 e2
  (Get _ _, Get _ _) -> True
  (Put _ _ _, Put _ _ _) -> True
  (ExtArray _, ExtArray _) -> True
  (ExtFunApp _ xs, ExtFunApp _ ys) -> length xs == length ys
  _ -> False

clean :: KNormal -> KNormal
clean e = case e of
  Neg _ -> Neg ""
  Add _ _ -> Add "" ""
  Sub _ _ -> Sub "" ""
  FNeg _ -> FNeg ""
  FAdd _ _ -> FAdd "" ""
  FSub _ _ -> FSub "" ""
  FMul _ _ -> FMul "" ""
  FDiv _ _ -> FDiv "" ""
  IfEq _ _ e1 e2 -> IfEq "" "" (clean e1) (clean e2)
  IfLE _ _ e1 e2 -> IfLE "" "" (clean e1) (clean e2)
  Let _ e1 e2 -> Let "" (clean e1) (clean e2)
  Var _ -> Var ""
  LetRec _ xs e1 e2 -> LetRec "" (map (const "") xs) (clean e1) (clean e2)
  App _ xs -> App "" (map (const "") xs)
  Tuple xs -> Tuple (map (const "") xs)
  LetTuple xs _ e -> LetTuple (map (const "") xs) "" (clean e)
  Get _ _ -> Get "" ""
  Put _ _ _ -> Put "" "" ""
  ExtArray _ -> ExtArray ""
  ExtFunApp _ xs -> ExtFunApp "" (map (const "") xs)
  e -> e

main :: IO ()
main = do
  str <- getLine
  case L.lex str of
    Nothing -> return ()
    Just lexemes -> do
      let x = fromJust . runGenT $ lift (P.parse lexemes) >>= nameTerm >>= typing >>= uncurry (convert S.empty)
      exp <- getLine
      if cmp x (read exp)
        then do
          putStrLn str
          main
        else do
          putStrLn $ "source:\n" ++ str
          putStrLn ""
          putStrLn $ "got:\n" ++ show (clean x)
          putStrLn ""
          putStrLn $ "expected:\n" ++ show (clean (read exp))
          undefined
