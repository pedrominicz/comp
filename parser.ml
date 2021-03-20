open Token_kind
(* Interestingly, you must open the module to type `infix.kind` instead of
 * `infix.Token.kind` and pattern match `kind` instead of `Token.kind`. *)
open Token

exception Parse_error of Token.t list

let error { kind; line } msg =
  if kind = EOF
    then Error.report line " at end" msg
    else Error.report line (" at '" ^ Token_kind.to_string kind ^ "'") msg

let rec synchronize = function
  | { kind = Semicolon } :: tokens -> tokens
  | { kind = (Class | EOF | For | Fun | If | Print | Return | Var | While) } :: _ as tokens ->
      tokens
  | _ :: tokens -> synchronize tokens
  | [] -> raise (Failure "Unreachable!")

let consume kind msg = function
  | token :: tokens when token.kind = kind -> tokens
  | token :: _ as tokens ->
      error token msg;
      raise (Parse_error tokens)
  | [] -> raise (Failure "Unreachable!")

let binary infixes k tokens =
  let rec loop left = function
    | infix :: tokens when List.mem infix.kind infixes ->
        let right, tokens = k tokens in
        loop (Ast.Binary (left, infix, right)) tokens
    | tokens -> (left, tokens) in
  let left, tokens = k tokens in
  loop left tokens

let rec expression tokens =
  equality tokens

and equality tokens =
  binary [Equal_equal; Bang_equal] comparison tokens

and comparison tokens =
  binary [Greater; Greater_equal; Less; Less_equal] term tokens

and term tokens =
  binary [Minus; Plus] factor tokens

and factor tokens =
  binary [Slash; Star] unary tokens

and unary = function
  | ({ kind = (Bang | Minus) } as prefix) :: tokens ->
      let right, tokens = unary tokens in
      (Ast.Unary (prefix, right), tokens)
  | tokens -> primary tokens

and primary = function
  | { kind = Number num } :: tokens -> (Ast.Literal (Ast.Number num), tokens)
  | { kind = String str } :: tokens -> (Ast.Literal (Ast.String str), tokens)
  | { kind = True } :: tokens -> (Ast.Literal (Ast.Bool true), tokens)
  | { kind = False } :: tokens -> (Ast.Literal (Ast.Bool false), tokens)
  | { kind = Nil } :: tokens -> (Ast.Literal Ast.Nil, tokens)
  | { kind = Left_paren } :: tokens ->
      let expr, tokens = expression tokens in
      let tokens = consume Right_paren "Expect ')' after expression." tokens in
      (expr, tokens)
  | { kind = Identifier name; line } :: tokens ->
      (Ast.Identifier (name, line), tokens)
  | token :: _ as tokens ->
      error token "Expect expression.";
      raise (Parse_error tokens)
  | [] -> raise (Failure "Unreachable!")

let statement = function
  | { kind = Print } :: tokens ->
      let expr, tokens = expression tokens in
      let tokens = consume Semicolon "Expect ';' after value." tokens in
      (Ast.Print expr, tokens)
  | tokens ->
      let expr, tokens = expression tokens in
      let tokens = consume Semicolon "Expect ';' after value." tokens in
      (Ast.Expression expr, tokens)

let variable_declaration = function
  | { kind = Identifier name; line } :: tokens ->
      let expr, tokens =
        match tokens with
        | { kind = Equal } :: tokens ->
            let expr, tokens = expression tokens in
            (Some expr, tokens)
        | tokens -> (None, tokens) in
      let tokens = consume Semicolon "Expect ';' after variable declaration." tokens in
      (Ast.Variable (name, line, expr), tokens)
  | token :: _ as tokens ->
      error token "Expect variable name.";
      raise (Parse_error tokens)
  | [] -> raise (Failure "Unreachable!")

let declaration tokens =
  try
    let stmt, tokens =
      match tokens with
      | { kind = Var } :: tokens -> variable_declaration tokens
      | tokens -> statement tokens in
    (Some stmt, tokens)
  with Parse_error tokens ->
    let tokens = synchronize tokens in
    (None, tokens)

let parse tokens =
  let rec loop statements = function
    | { kind = EOF } :: _ -> List.rev statements
    | tokens ->
        match declaration tokens with
        | Some stmt, tokens -> loop (stmt :: statements) tokens
        | None, tokens -> loop statements tokens in
  loop [] tokens
