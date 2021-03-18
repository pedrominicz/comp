type literal =
  | Number of float
  | String of string
  | Bool of bool
  | Nil

let literal_to_string = function
  | Number num -> Float.to_string num
  | String str -> "\"" ^ str ^ "\""
  | Bool bool -> Bool.to_string bool
  | Nil -> "nil"

type expression =
  | Literal of literal
  | Unary of Token.t * expression
  | Binary of expression * Token.t * expression

let rec expression_to_string = function
  | Literal lit -> literal_to_string lit
  | Unary (operator, right) ->
      "(" ^ Token.to_string operator ^ expression_to_string right ^ ")"
  | Binary (left, operator, right) ->
      "(" ^ expression_to_string left ^ " " ^ Token.to_string operator ^ " " ^ expression_to_string right ^ ")"
