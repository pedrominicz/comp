{
module Lexer (Lexeme(..), Lexer.lex) where
}

%wrapper "basic"

$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]

tokens :-
  $white+       ;
  --"(*"          { undefined }
  "("           { \s -> LParen }
  ")"           { \s -> RParen }
  "true"        { \s -> Bool True }
  "false"       { \s -> Bool False }
  "not"         { \s -> Not }
  $digit+       { \s -> Int (read s) }
  $digit+ "." $digit* { float }
  "-"           { \s -> Minus }
  "+"           { \s -> Plus }
  "-."          { \s -> MinusDot }
  "+."          { \s -> PlusDot }
  "*."          { \s -> AstDot}
  "/."          { \s -> SlashDot }
  "="           { \s -> Equal }
  "<>"          { \s -> LessGreater }
  "<="          { \s -> LessEqual }
  ">="          { \s -> GreaterEqual }
  "<"           { \s -> Less }
  ">"           { \s -> Greater }
  "if"          { \s -> If }
  "then"        { \s -> Then }
  "else"        { \s -> Else }
  "let"         { \s -> Let }
  "in"          { \s -> In }
  "rec"         { \s -> Rec }
  ","           { \s -> Comma }
  "_"           { \s -> Ident "" }
  "Array.create" | "Array.make" { \s -> ArrayCreate }
  "."           { \s -> Dot }
  "<-"          { \s -> LessMinus }
  ";"           { \s -> Semicolon }
  $lower ($digit | $lower | $upper | "_")* { \s -> Ident s }

{
data Lexeme
  = Bool Bool
  | Int Int
  | Float Float
  | Not
  | Minus
  | Plus
  | MinusDot
  | PlusDot
  | AstDot
  | SlashDot
  | Equal
  | LessGreater
  | LessEqual
  | GreaterEqual
  | Less
  | Greater
  | If
  | Then
  | Else
  | Ident String
  | Let
  | In
  | Rec
  | Comma
  | ArrayCreate
  | Dot
  | LessMinus
  | Semicolon
  | LParen
  | RParen
  deriving Show

float :: String -> Lexeme
float str =
  if last str == '.'
    then Float (read (init str))
    else Float (read str)

lex :: String -> Maybe [Lexeme]
lex str = go ('\n', [], str)
  where
  go input@(_, _, str) =
    case alexScan input 0 of
      AlexEOF -> Just []
      AlexError _ -> Nothing
      AlexSkip input _ -> go input
      AlexToken input len action -> (action (take len str) :) <$> go input
}
