open Token
open Value

exception Runtime_error of Token.t * string
exception Return of Value.t * int

let evaluate_binary_number op left token right =
  match (left, right) with
  | Number left, Number right -> op left right
  | _ -> raise (Runtime_error (token, "Operands must be numbers."))

let rec evaluate = function
  | Ast.Literal literal -> Value.of_literal literal
  | Ast.Unary (token, right) -> evaluate_unary token right
  | Ast.Binary (left, token, right) -> evaluate_binary left token right
  | Ast.LazyBinary (left, token, right) -> evaluate_lazy_binary left token right
  | Ast.Identifier (name, line) ->
      let token = Token.make (Token_kind.Identifier name) line in
      let msg = "Undefined variable '" ^ name ^ "'." in
      let exn () = raise (Runtime_error (token, msg)) in
      Environment.get name exn
  | Ast.Grouping expr -> evaluate expr
  | Ast.Assignment (name, line, expr) ->
      let value = evaluate expr in
      let token = Token.make (Token_kind.Identifier name) line in
      let msg = "Undefined variable '" ^ name ^ "'." in
      let exn () = raise (Runtime_error (token, msg)) in
      Environment.assign name value exn;
      value
  | Ast.Call (callee, token, arguments) ->
      let callee = evaluate callee in
      let arguments = List.map evaluate arguments in
      match callee with
      | Callable (arity, name, callable) ->
          if List.length arguments <> arity
          then raise (Runtime_error (token, "Expected " ^ Int.to_string arity ^ " arguments but got " ^ Int.to_string (List.length arguments) ^ "."));
          callable arguments
      | _ -> raise (Runtime_error (token, "Can only call functions and classes."))

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

and evaluate_lazy_binary left token right =
  let left = evaluate left in
  match token.kind with
  | Or -> if Value.is_truthy left then left else evaluate right
  | And -> if not (Value.is_truthy left) then left else evaluate right
  | _ -> raise (Failure "Unreachable!")

let rec execute = function
  | Ast.Expression expr -> let _ = evaluate expr in ()
  | Ast.Print expr -> print_endline (Value.to_string (evaluate expr))
  | Ast.Variable (name, line, expr) ->
      let expr = match expr with Some expr -> evaluate expr | None -> Nil in
      Environment.define name expr
  | Ast.Block stmts ->
      Environment.push ();
      List.iter execute stmts;
      Environment.pop ()
  | Ast.If (condition, then_branch, else_branch) ->
      if Value.is_truthy (evaluate condition)
        then execute then_branch
        else Option.iter execute else_branch
  | Ast.While (condition, body) ->
      while Value.is_truthy (evaluate condition) do
        execute body
      done
  | Ast.Function (name, args, body) ->
      let arity = List.length args in
      let callable args' =
        let old_env = !Environment.env in
        Environment.push ();
        List.iter2 Environment.define args args';
        let return_value =
          try
            List.iter execute body;
            Value.Nil
          with Return (return_value, _) ->
            return_value in
        Environment.env := old_env;
        return_value in
      Environment.define name (Value.Callable (arity, name, callable))
  | Ast.Return (expr, line) -> raise (Return (evaluate expr, line))

let interpret stmts =
  try
    List.iter execute stmts
  with
  | Runtime_error (token, msg) ->
      Error.report token.line (" at '" ^ Token.to_string token ^ "'") msg
  | Return (_, line) ->
      Error.error line "Unexpected return statement."
