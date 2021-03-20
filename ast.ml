type literal =
  | Number of float
  | String of string
  | Bool of bool
  | Nil

type expression =
  | Literal of literal
  | Unary of Token.t * expression
  | Binary of expression * Token.t * expression
  | Identifier of string * int

type statement =
  | Expression of expression
  | Print of expression
  | Variable of string * int * expression option
