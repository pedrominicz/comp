module Convert where

import qualified Instruction as I
import qualified Syntax as S

convertExpr :: S.Expr -> Maybe [I.Instruction]
convertExpr e = case e of
  S.Id i -> undefined
  S.Int x -> return $ [I.Int x]
  S.Call f xs -> undefined
  S.Add x y -> do
    x <- convertExpr x
    y <- convertExpr y
    return $ x ++ y ++ [I.Add]
  S.Assign x y -> undefined
