open Token_kind
(* Interestingly, you must open the module to type `infix.kind` instead of
 * `infix.Token.kind` and pattern match `kind` instead of `Token.kind`. *)
open Token

exception Parse_error of Token.t list

let error { kind; line } msg =
  if kind = EOF
    then Error.report line " at end" msg
    else Error.report line (" at '" ^ Token_kind.to_string kind ^ "'") msg

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
  | token :: _ as tokens ->
      error token "Expect expression.";
      raise (Parse_error tokens)
  | [] -> raise (Failure "Unreachable!")

let parse tokens =
  try let expr, _ = expression tokens in Some expr with Parse_error _ -> None
