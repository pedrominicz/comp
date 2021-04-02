{
module Parser where

import qualified Lexer as L
import qualified Syntax as S
import qualified Type as T

import Data.Maybe
}

%tokentype { L.Lexeme }

%monad { Either (Maybe L.Lexeme) }
%error { parseError }

%name parse exp

%token
  bool          { (_, _, _, L.Bool $$) }
  int           { (_, _, _, L.Int $$) }
  float         { (_, _, _, L.Float $$) }
  'not'         { (_, _, _, L.Not) }
  '-'           { (_, _, _, L.Minus) }
  '+'           { (_, _, _, L.Plus) }
  '-.'          { (_, _, _, L.MinusDot) }
  '+.'          { (_, _, _, L.PlusDot) }
  '*.'          { (_, _, _, L.AstDot) }
  '/.'          { (_, _, _, L.SlashDot) }
  '='           { (_, _, _, L.Equal) }
  '<>'          { (_, _, _, L.LessGreater) }
  '<='          { (_, _, _, L.LessEqual) }
  '>='          { (_, _, _, L.GreaterEqual) }
  '<'           { (_, _, _, L.Less) }
  '>'           { (_, _, _, L.Greater) }
  'if'          { (_, _, _, L.If) }
  'then'        { (_, _, _, L.Then) }
  'else'        { (_, _, _, L.Else) }
  ident         { (_, _, _, L.Ident $$) }
  'let'         { (_, _, _, L.Let) }
  'in'          { (_, _, _, L.In) }
  'rec'         { (_, _, _, L.Rec) }
  '_'           { (_, _, _, L.Underscore) }
  ','           { (_, _, _, L.Comma) }
  'Array.create' { (_, _, _, L.ArrayCreate) }
  '.'           { (_, _, _, L.Dot) }
  '<-'          { (_, _, _, L.LessMinus) }
  ';'           { (_, _, _, L.Semicolon) }
  '('           { (_, _, _, L.LParen) }
  ')'           { (_, _, _, L.RParen) }
%%

exp :: { S.Syntax }
  : exp_                        { $1 }
  | exp_ ';' exp                { S.Let Nothing T.Unit $1 $3 }
  | 'let' '_' '=' exp 'in' exp  { S.Let Nothing (T.Var Nothing) $4 $6 }
  | 'let' ident '=' exp 'in' exp { S.Let (Just $2) (T.Var Nothing) $4 $6 }

exp_ :: { S.Syntax }
  : tuple_exp                   { $1 }
  | simple_exp '.' '(' exp ')' '<-' exp_ { S.Put $1 $4 $7 }
  | 'if' exp 'then' exp 'else' exp_ { S.If $2 $4 $6 }

tuple_exp :: { S.Syntax }
  : eq_exp                      { $1 }
  | elems                       { S.Tuple (reverse $1) }

elems :: { [S.Syntax] }
  : elems ',' eq_exp            { $3 : $1 }
  | eq_exp ',' eq_exp           { [$3, $1] }

eq_exp :: { S.Syntax }
  : add_exp                     { $1 }
  | eq_exp '=' add_exp          { S.Eq $1 $3 }
  | eq_exp '<>' add_exp         { S.Not (S.Eq $1 $3) }
  | eq_exp '<=' add_exp         { S.LE $1 $3 }
  | eq_exp '>=' add_exp         { S.LE $3 $1 }
  | eq_exp '<' add_exp          { S.Not (S.LE $3 $1) }
  | eq_exp '>' add_exp          { S.Not (S.LE $1 $3) }

add_exp :: { S.Syntax }
  : mul_exp                     { $1 }
  | add_exp '+' mul_exp         { S.Add $1 $3 }
  | add_exp '-' mul_exp         { S.Sub $1 $3 }
  | add_exp '+.' mul_exp        { S.FAdd $1 $3 }
  | add_exp '-.' mul_exp        { S.FSub $1 $3 }

mul_exp :: { S.Syntax }
  : neg_exp                     { $1 }
  | mul_exp '*.' neg_exp        { S.FMul $1 $3 }
  | mul_exp '/.' neg_exp        { S.FDiv $1 $3 }

neg_exp :: { S.Syntax }
  : app_exp                     { $1 }
  -- `floatAux` handles negative float literals, e.g. "-2.".
  | '-' neg_exp                 { floatAux $2 }
  | '-.' neg_exp                { S.FNeg $2 }

app_exp :: { S.Syntax }
  : simple_exp                  { $1 }
  | simple_exp actual_args      { S.App $1 (reverse $2) }
  | 'Array.create' simple_exp simple_exp { S.Array $2 $3 }

actual_args :: { [S.Syntax] }
  : simple_exp                  { [$1] }
  | actual_args simple_exp      { $2 : $1 }

simple_exp :: { S.Syntax }
  : '(' exp ')'                 { $2 }
  | '(' ')'                     { S.Unit }
  | bool                        { S.Bool $1 }
  | int                         { S.Int $1 }
  | float                       { S.Float $1 }
  | ident                       { S.Var $1 }
  | simple_exp '.' '(' exp ')'  { S.Get $1 $4 }

{
parseError :: [L.Lexeme] -> Either (Maybe L.Lexeme) a
parseError = Left . listToMaybe

floatAux :: S.Syntax -> S.Syntax
floatAux (S.Float float) = S.Float (-float)
floatAux exp = S.Neg exp
}
