{
module Parser (parse) where

import qualified Lexer as L
import qualified Syntax as S
import qualified Type as T
}

%expect 0

%tokentype { L.Lexeme }

%monad { Maybe }
%error { const Nothing }

%name parse exp

%token
  bool          { L.Bool $$ }
  int           { L.Int $$ }
  float         { L.Float $$ }
  'not'         { L.Not }
  '-'           { L.Minus }
  '+'           { L.Plus }
  '-.'          { L.MinusDot }
  '+.'          { L.PlusDot }
  '*.'          { L.AstDot }
  '/.'          { L.SlashDot }
  '='           { L.Equal }
  '<>'          { L.LessGreater }
  '<='          { L.LessEqual }
  '>='          { L.GreaterEqual }
  '<'           { L.Less }
  '>'           { L.Greater }
  'if'          { L.If }
  'then'        { L.Then }
  'else'        { L.Else }
  ident         { L.Ident $$ }
  'let'         { L.Let }
  'in'          { L.In }
  'rec'         { L.Rec }
  ','           { L.Comma }
  'Array.create' { L.ArrayCreate }
  '.'           { L.Dot }
  '<-'          { L.LessMinus }
  ';'           { L.Semicolon }
  '('           { L.LParen }
  ')'           { L.RParen }
%%

exp :: { S.Syntax }
  : let_exp %shift              { $1 }
  | let_exp ';' exp             { S.Let "" T.Unit $1 $3 }

let_exp :: { S.Syntax }
  : tuple_exp                   { $1 }
  | simple_exp '.' '(' exp ')' '<-' let_exp
                                { S.Put $1 $4 $7 }
  | 'if' exp 'then' exp 'else' let_exp
                                { S.If $2 $4 $6 }
  | 'let' ident '=' exp 'in' exp
                                { S.Let $2 (T.Var 0) $4 $6 }
  | 'let' '(' pat ')' '=' exp 'in' exp
                                { S.LetTuple (reverse $3) $6 $8 }
  | 'let' 'rec' ident formal_args '=' exp 'in' exp
                                { letRecAux $3 $4 $6 $8 }

pat :: { [(String, T.Type)] }
  : pat ',' ident               { ($3, T.Var 0) : $1 }
  | ident ',' ident             { [($3, T.Var 0), ($1, T.Var 0)] }

formal_args :: { [(String, T.Type)] }
  : ident                       { [($1, T.Var 0)] }
  | formal_args ident           { ($2, T.Var 0) : $1 }

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
  | 'not' simple_exp            { S.Not $2 }
  | 'Array.create' simple_exp simple_exp
                                { S.Array $2 $3 }

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
letRecAux :: String -> [(String, T.Type)] -> S.Syntax -> S.Syntax -> S.Syntax
letRecAux name args body exp = S.LetRec name (T.Var 0) (reverse args) body exp

floatAux :: S.Syntax -> S.Syntax
floatAux (S.Float float) = S.Float (-float)
floatAux exp = S.Neg exp
}
