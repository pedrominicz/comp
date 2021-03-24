type t =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
  | Callable of int * string * (t list -> t)

let to_string = function
  | Number num -> Util.float_to_string num
  | String str -> str
  | Bool bool -> Bool.to_string bool
  | Nil -> "nil"
  | Callable (_, "clock", _) -> "<native fn>"
  | Callable (_, name, _) -> "<fn " ^ name ^ ">"

let is_truthy = function
  | Nil | Bool false -> false
  | _ -> true

let of_literal = function
  | Ast.Number num -> Number num
  | Ast.String str -> String str
  | Ast.Bool bool -> Bool bool
  | Ast.Nil -> Nil
