type literal =
  | Number of float
  | String of string
  | Bool of bool
  | Nil

type expression =
  | Literal of literal
  | Unary of Token.t * expression
  | Binary of expression * Token.t * expression
  | LazyBinary of expression * Token.t * expression
  | Identifier of string * int
  | Grouping of expression
  | Assignment of string * int * expression

type statement =
  | Expression of expression
  | Print of expression
  | Variable of string * int * expression option
  | Block of statement list
  | If of expression * statement * statement option
  | While of expression * statement
