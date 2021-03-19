open Token
open Value

exception Runtime_error of Token.t * string

let evaluate_binary_number op left token right =
  match (left, right) with
  | Number left, Number right -> op left right
  | _ -> raise (Runtime_error (token, "Operands must be numbers."))

let rec evaluate = function
  | Ast.Literal literal -> Value.of_literal literal
  | Ast.Unary (token, right) -> evaluate_unary token right
  | Ast.Binary (left, token, right) -> evaluate_binary left token right

and evaluate_unary token right =
  let right = evaluate right in
  match (token.kind, right) with
  | Minus, Number num -> Number (-. num)
  | Minus, _ -> raise (Runtime_error (token, "Operand must be a number."))
  | Bang, _ -> Bool (not (Value.is_truthy right))
  | _ -> raise (Failure "Unreachable!")

and evaluate_binary left token right =
  let left = evaluate left in
  let right = evaluate right in
  match token.kind with
  | Bang_equal -> Bool (left <> right)
  | Equal_equal -> Bool (left = right)
  | Greater -> Bool (evaluate_binary_number (>) left token right)
  | Greater_equal -> Bool (evaluate_binary_number (>=) left token right)
  | Less -> Bool (evaluate_binary_number (<) left token right)
  | Less_equal -> Bool (evaluate_binary_number (<=) left token right)
  | Minus -> Number (evaluate_binary_number Float.sub left token right)
  | Slash -> Number (evaluate_binary_number Float.div left token right)
  | Star -> Number (evaluate_binary_number Float.mul left token right)
  | Plus ->
      (match (left, right) with
      | Number left, Number right -> Number (Float.add left right)
      | String left, String right -> String (left ^ right)
      | _ -> raise (Runtime_error (token, "Operands must be two numbers or two strings.")))
  | _ -> raise (Failure "Unreachable!")

let interpret expr =
  try
    let value = evaluate expr in
    print_endline (Value.to_string value)
  with Runtime_error (token, msg) ->
    Error.report token.line (" at '" ^ Token.to_string token ^ "'") msg
