module Convert where

import Instruction (Instruction)
import qualified Instruction as I
import Syntax (Expr)
import qualified Syntax as S

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

convertExpr :: Expr -> [Instruction]
-- Primary expressions
convertExpr (S.Identifier x) = undefined
convertExpr (S.IntLiteral x) = [I.Int x]
convertExpr (S.StringLiteral s) = undefined
-- Postfix expresssions
convertExpr (S.FunctionCall f xs) = let
  args = concat [convertExpr x | x <- xs]
  in args ++ convertExpr f ++ [I.Call (length xs)]
-- Unary expressions
convertExpr (S.AddressOf e) = undefined
convertExpr (S.Indirection e) = undefined
convertExpr (S.Negative e) = convertExpr e ++ [I.Neg]
convertExpr (S.BitwiseNot e) = convertExpr e ++ [I.BitwiseNot]
convertExpr (S.Not e) = convertExpr e ++ [I.Not]
-- Binary expressions   
convertExpr (S.Mul e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.Mul]
convertExpr (S.Div e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.Div]
convertExpr (S.Mod e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.Mod]
convertExpr (S.Add e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.Add]
convertExpr (S.Sub e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.Sub]
convertExpr (S.ShiftLeft e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.ShiftLeft]
convertExpr (S.ShiftRight e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.ShiftRight]
convertExpr (S.LT' e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.LT']
convertExpr (S.GT' e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.GT']
convertExpr (S.LE e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.LE]
convertExpr (S.GE e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.GE]
convertExpr (S.EQ' e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.EQ']
convertExpr (S.NE e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.NE]
convertExpr (S.BitwiseAnd e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.BitwiseAnd]
convertExpr (S.BitwiseXor e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.BitwiseXor]
convertExpr (S.BitwiseOr e1 e2) = convertExpr e1 ++ convertExpr e1 ++ [I.BitwiseOr]
-- Assigment expressions
convertExpr (S.Assignment s e) = undefined
