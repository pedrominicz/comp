module ANF (normalize, normalize') where

import Expr
import qualified Syntax as S

import Control.Monad.State
import Data.List
import qualified Data.ByteString as B

-- Administrative normal form (ANF) expressions have to satisfy the constraints
-- imposed by the two grammars below. Each grammar reveals some structure not
-- so apparent in the other.
--
-- Grammar based on The Essence of Compiling with Continuations:
--
--  <expr> ::= <value>
--           | 'let' <var> '=' <value> 'in' <expr>
--           | <value> <value>
--           | 'let' <var> '=' <value> <value> 'in' <expr>
--
--  <value> ::= <var>
--            | 'λ' <var> ',' <expr>
--
-- Let expressions can only bind variables, applications and primitive
-- operations (not present in the grammar above), in particular, let
-- expressions cannot be let bound.
--
-- Grammar based on `https://matt.might.net/articles/a-normalization/`:
--
--  <aexpr> ::= <var>
--            | 'λ' <var> ',' <expr>
--
--  <cexpr> ::= <aexpr> <aexpr>
--
--  <expr> ::= 'let' <var> '=' <expr> 'in' <expr>
--           | <aexpr>
--           | <cexpr>
--
-- Evaluating an atomic expression (`<aexpr>`) is guaranteed to terminate,
-- cause no side effects, cause no control effects and never produce an error.
-- A lambda expression is atomic because evaluating it creates a closure and
-- that process satisfies the required constraints.
--
-- Evaluating a complex expression (`<cexpr>`) may not terminate, may have a
-- side effect and may produce an error. However, a complex expression may
-- defer execution to only one other complex expression. For example, the
-- following are also complex expressions:
--
--  <cexpr> ::= <aexpr> <aexpr>
--            | 'if' <aexpr> 'then' <expr> 'else' <expr>
--            | 'let' 'rec' <var> '=' <aexpr> 'in' <expr>
--
-- Although function application can apply any two values, the normalizer below
-- only allows applying variables to variables. This is similar to the
-- continuation passing style (CPS) grammars in Compiling with Continuations,
-- Continued.

fresh :: Monad m => StateT Int m S.Name
fresh = state $ \x -> ((B.empty, x), x + 1)

nameless :: S.Expr -> Maybe Expr
nameless = go []
  where
  go :: [S.Name] -> S.Expr -> Maybe Expr
  go ctx (S.Var x) = Var <$> elemIndex x ctx
  go ctx (S.Lam x b) = Lam <$> go (x : ctx) b
  go ctx (S.App (S.Var f) (S.Var a)) =
    App <$> elemIndex f ctx <*> elemIndex a ctx
  go _ (S.App _ _) = error "unreachable"
  go ctx (S.Let x e1 e2) = Let <$> go ctx e1 <*> go (x : ctx) e2

normalize :: S.Expr -> Maybe Expr
normalize e = nameless $ evalState (expr e) 0
  where
  expr :: S.Expr -> State Int S.Expr
  expr (S.Var x) = return $ S.Var x
  expr (S.Lam x b) = S.Lam x <$> expr b
  expr (S.App f a) =
    name f $ \f ->
      name a $ \a ->
        return $ S.App (S.Var f) (S.Var a) 
  expr (S.Let x e1 e2) = do
    e1 <- expr e1
    e2 <- expr e2
    -- As pointed out in Compiling with Continuations, Continued, the
    -- A-normalization algorithm in The Essence of Compiling with Continuations
    -- doesn't actually normalizes terms, as it doesn't deal with the let
    -- expression invariant.
    case e1 of
      S.Let x' e1' e2' -> return $ S.Let x' e1' (S.Let x e2' e2)
      _ -> return $ S.Let x e1 e2

  name :: S.Expr -> (S.Name -> State Int S.Expr) -> State Int S.Expr
  name e k = do
    e <- expr e
    case e of
      S.Var x -> k x
      _ -> do
        x <- fresh
        S.Let x e <$> k x

type M a = StateT Int Maybe Expr

normalize' :: S.Expr -> Maybe Expr
normalize' e = evalStateT (expr [] e) 0
  where
  expr :: [S.Name] -> S.Expr -> M Expr
  expr ctx (S.Var x) = lift $ Var <$> elemIndex x ctx
  expr ctx (S.Lam x b) = Lam <$> expr (x : ctx) b
  expr ctx (S.App f a) =
    name ctx f $ \ctx f ->
      name ctx a $ \ctx a ->
        lift $ App <$> elemIndex f ctx <*> elemIndex a ctx
  expr ctx (S.Let x e1 e2) = do
    e1 <- expr ctx e1
    case e1 of
      Let e1' e2' -> Let e1' . Let e2' <$> expr (x : dummy : ctx) e2
      _ -> Let e1 <$> expr (x : ctx) e2

  name :: [S.Name] -> S.Expr -> ([S.Name] -> S.Name -> M Expr) -> M Expr
  name ctx e k = do
    e <- expr ctx e
    case e of
      Var x -> k ctx (ctx !! x)
      _ -> do
        x <- fresh
        Let e <$> k (x : ctx) x

  dummy :: S.Name
  dummy = (B.empty, 0)
