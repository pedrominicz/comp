type t =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
  | Function of string * string list * Ast.statement list * (string, t) Hashtbl.t

let to_string = function
  | Number num -> Util.float_to_string num
  | String str -> str
  | Bool bool -> Bool.to_string bool
  | Nil -> "nil"
  | Function ("clock", _, _, _) -> "<native fn>"
  | Function (name, _, _, _) -> "<fn " ^ name ^ ">"

let is_truthy = function
  | Nil | Bool false -> false
  | _ -> true

let of_literal = function
  | Ast.Number num -> Number num
  | Ast.String str -> String str
  | Ast.Bool bool -> Bool bool
  | Ast.Nil -> Nil
