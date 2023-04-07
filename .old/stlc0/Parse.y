{
module Parse (parse) where

import Syntax
import qualified Lex as L

import Data.ByteString (ByteString)
}

%expect 0

%tokentype { L.Token }

%monad { Maybe }
%error { const Nothing }

%name expr expr

%token
  num           { L.Num $$ }
  var           { L.Var $$ }
  '='           { L.Equal }
  ','           { L.Comma }
  '('           { L.LParen }
  ')'           { L.RParen }
  'λ'           { L.Lam }
  'let'         { L.Let }
  'in'          { L.In }
  '+'           { L.Plus }
%%

-- Expressions

expr :: { Expr }
  : 'λ' args1 ',' expr            { foldr Lam $4 $2 }
  | 'let' var '=' expr 'in' expr  { Let ($2, 0) $4 $6 }
  | addition                      { $1 }

addition :: { Expr }
  : addition '+' application      { Add $1 $3 }
  | application                   { $1 }

application :: { Expr }
  : application simple            { App $1 $2 }
  | simple                        { $1 }

simple :: { Expr }
  : num                           { Num $1 }
  | var                           { Var ($1, 0) }
  | '(' expr ')'                  { $2 }

args1 :: { [Name] }
  : var                           { [($1, 0)] }
  | var args1                     { ($1, 0) : $2 }

{
parse :: ByteString -> Maybe Expr
parse str = L.lexer str >>= expr
}
