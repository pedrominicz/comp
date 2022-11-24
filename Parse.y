{
module Parse (parse) where

import qualified Expr as E
import qualified Lex as L

import Data.ByteString (ByteString)
import Data.List
}

%expect 0

%tokentype { L.Token }

%monad { Maybe }
%error { const Nothing }

%name expr expr

%token
  var           { L.Var $$ }
  '='           { L.Equal }
  ','           { L.Comma }
  '('           { L.LParen }
  ')'           { L.RParen }
  'λ'           { L.Lam }
  'let'         { L.Let }
  'in'          { L.In }
%%

-- Expressions

expr :: { Expr }
  : 'λ' arguments ',' expr        { foldr Lam $4 $2 }
  | 'let' var '=' expr 'in' expr  { Let $2 $4 $6 }
  | application                   { $1 }

application :: { Expr }
  : application simple            { App $1 $2 }
  | simple                        { $1 }

simple :: { Expr }
  : var                           { Var $1 }
  | '(' expr ')'                  { $2 }

arguments :: { [ByteString] }
  : var                           { [$1] }
  | var arguments                 { $1 : $2 }

{
data Expr
  = Var {-# UNPACK #-} !ByteString
  | Lam {-# UNPACK #-} !ByteString Expr
  | App Expr Expr
  | Let {-# UNPACK #-} !ByteString Expr Expr

nameless :: Expr -> Maybe E.Expr
nameless = go []
  where
  go :: [ByteString] -> Expr -> Maybe E.Expr
  go ctx (Var x) = E.Var <$> elemIndex x ctx
  go ctx (Lam x b) = E.Lam <$> go (x : ctx) b
  go ctx (App f a) = E.App <$> go ctx f <*> go ctx a
  go ctx (Let x e1 e2) = E.Let <$> go ctx e1 <*> go (x : ctx) e2

parse :: ByteString -> Maybe E.Expr
parse str = L.lexer str >>= expr >>= nameless
}
