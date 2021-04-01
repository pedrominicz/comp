{
module Parse where

import qualified Lex as L
import qualified Syntax as S

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
  identifier    { (_, _, _, L.Ident $$) }
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
  : add_exp                     { $1 }

add_exp :: { S.Syntax }
  : mul_exp                     { $1 }
  | add_exp '+' mul_exp         { S.Add $1 $3 }
  | add_exp '-' mul_exp         { S.Sub $1 $3 }
  | add_exp '+.' mul_exp        { S.FAdd $1 $3 }
  | add_exp '-.' mul_exp        { S.FSub $1 $3 }

mul_exp :: { S.Syntax }
  : simple_exp                  { $1 }
  | mul_exp '*.' simple_exp     { S.FMul $1 $3 }
  | mul_exp '/.' simple_exp     { S.FDiv $1 $3 }

simple_exp :: { S.Syntax }
  : '(' exp ')'                 { $2 }
  | '(' ')'                     { S.Unit }
  | bool                        { S.Bool $1 }
  | int                         { S.Int $1 }
  | float                       { S.Float $1 }
  | identifier                  { S.Var $1 }
  | simple_exp '.' '(' exp ')'  { S.Get $1 $4 }

{
parseError :: [L.Lexeme] -> Either (Maybe L.Lexeme) a
parseError = Left . listToMaybe
}
