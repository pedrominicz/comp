open Token_kind
(* Interestingly, you must open the module to type `infix.kind` instead of
 * `infix.Token.kind` and pattern match `kind` instead of `Token.kind`. *)
open Token

exception Parse_error of Token.t list

let aux = List.iter (fun token -> print_endline (Token.to_string token))

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
  assignment tokens

and assignment tokens =
  let expr, tokens = logic_or tokens in
  match tokens with
  | ({ kind = Equal } as token) :: tokens ->
      let value, tokens = assignment tokens in
      (match expr with
      | Ast.Identifier (name, line) -> 
          (Ast.Assignment (name, line, value), tokens)
      | expr ->
          error token "Invalid assignment target.";
          (expr, tokens))
  | _ -> (expr, tokens)

and logic_or tokens =
  let rec loop left = function
    | ({ kind = Or } as token) :: tokens ->
        let right, tokens = logic_and tokens in
        loop (Ast.LazyBinary (left, token, right)) tokens
    | tokens -> (left, tokens) in
  let left, tokens = logic_and tokens in
  loop left tokens

and logic_and tokens =
  let rec loop left = function
    | ({ kind = And } as token) :: tokens ->
        let right, tokens = equality tokens in
        loop (Ast.LazyBinary (left, token, right)) tokens
    | tokens -> (left, tokens) in
  let left, tokens = equality tokens in
  loop left tokens

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
  | tokens -> call tokens

and call tokens =
  let rec loop expr = function
    | { kind = Left_paren } :: tokens ->
        let expr, tokens = finish_call expr tokens in
        loop expr tokens
    | tokens -> (expr, tokens) in
  let expr, tokens = primary tokens in
  loop expr tokens

and finish_call callee tokens =
  let rec get_arguments args tokens =
    let arg, tokens = expression tokens in
    let args = arg :: args in
    match tokens with
    | { kind = Comma } :: tokens -> get_arguments args tokens
    | tokens -> (List.rev args, tokens) in
  let arguments, tokens =
    match tokens with
    | { kind = Right_paren } :: _ as tokens -> ([], tokens)
    | tokens -> get_arguments [] tokens in
  (* Dumb hack because `consume` doesn't return the token it matches. *)
  let _ = consume Right_paren "Expect ')' after arguments." tokens in
  (Ast.Call (callee, List.hd tokens, arguments), List.tl tokens)

and primary = function
  | { kind = Number num } :: tokens -> (Ast.Literal (Ast.Number num), tokens)
  | { kind = String str } :: tokens -> (Ast.Literal (Ast.String str), tokens)
  | { kind = True } :: tokens -> (Ast.Literal (Ast.Bool true), tokens)
  | { kind = False } :: tokens -> (Ast.Literal (Ast.Bool false), tokens)
  | { kind = Nil } :: tokens -> (Ast.Literal Ast.Nil, tokens)
  | { kind = Left_paren } :: tokens ->
      let expr, tokens = expression tokens in
      let tokens = consume Right_paren "Expect ')' after expression." tokens in
      (Ast.Grouping expr, tokens)
  | { kind = Identifier name; line } :: tokens ->
      (Ast.Identifier (name, line), tokens)
  | token :: _ as tokens ->
      error token "Expect expression.";
      raise (Parse_error tokens)
  | [] -> raise (Failure "Unreachable!")

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

let rec declaration tokens =
  try
    let stmt, tokens =
      match tokens with
      | { kind = Var } :: tokens -> variable_declaration tokens
      | tokens -> statement tokens in
    (Some stmt, tokens)
  with Parse_error tokens ->
    let tokens = synchronize tokens in
    (None, tokens)

and statement = function
  | { kind = Print } :: tokens ->
      let expr, tokens = expression tokens in
      let tokens = consume Semicolon "Expect ';' after value." tokens in
      (Ast.Print expr, tokens)
  | { kind = Left_brace } :: tokens ->
      let stmts, tokens = block [] tokens in
      (Ast.Block stmts, tokens)
  | { kind = If } :: tokens -> if_statement tokens
  | { kind = While } :: tokens -> while_statement tokens
  | { kind = For } :: tokens -> for_statement tokens
  | { kind = Fun } :: tokens -> parse_function "function" tokens
  | { kind = Return; line } :: tokens -> parse_return line tokens
  | tokens -> expression_statement tokens

and expression_statement tokens =
  let expr, tokens = expression tokens in
  let tokens = consume Semicolon "Expect ';' after value." tokens in
  (Ast.Expression expr, tokens)

and block stmts = function
  | { kind = (Right_brace | EOF) } :: _ as tokens ->
      let tokens = consume Right_brace "Expect '}' after block." tokens in
      (List.rev stmts, tokens)
  | tokens ->
      let stmt, tokens = declaration tokens in
      match stmt with
      | Some stmt -> block (stmt :: stmts) tokens
      | None -> block stmts tokens

and if_statement tokens =
  let tokens = consume Left_paren "Expect '(' after 'if'." tokens in
  let condition, tokens = expression tokens in
  let tokens = consume Right_paren "Expect ')' after if condition." tokens in
  let then_branch, tokens = statement tokens in
  match tokens with
  | { kind = Else } :: tokens ->
      let else_branch, tokens = statement tokens in
      (Ast.If (condition, then_branch, Some else_branch), tokens)
  | tokens -> (Ast.If (condition, then_branch, None), tokens)

and while_statement tokens =
  let tokens = consume Left_paren "Expect '(' after 'if'." tokens in
  let condition, tokens = expression tokens in
  let tokens = consume Right_paren "Expect ')' after if condition." tokens in
  let body, tokens = statement tokens in
  (Ast.While (condition, body), tokens)

and for_statement tokens =
  let tokens = consume Left_paren "Expect '(' after 'if'." tokens in
  let init, tokens =
    match tokens with
    | { kind = Semicolon } :: tokens -> (None, tokens)
    | { kind = Var } :: tokens ->
        let init, tokens = variable_declaration tokens in
        (Some init, tokens)
    | tokens ->
        let init, tokens = expression_statement tokens in
        (Some init, tokens) in
  let condition, tokens =
    match tokens with
    | { kind = Semicolon } :: tokens -> (None, tokens)
    | tokens ->
        let condition, tokens = expression tokens in
        let tokens = consume Semicolon "Expect ';' after loop condition." tokens in
        (Some condition, tokens) in
  let increment, tokens =
    match tokens with
    | { kind = Right_paren } :: tokens -> (None, tokens)
    | tokens ->
        let increment, tokens = expression tokens in
        let tokens = consume Right_paren "Expect ')' after for clauses." tokens in
        (Some increment, tokens) in
  let body, tokens = statement tokens in
  let body =
    match increment with
    | Some increment -> Ast.Block [body; Ast.Expression increment]
    | None -> body in
  let body =
    match condition with
    | Some condition -> Ast.While (condition, body)
    | None -> Ast.While (Ast.Literal (Ast.Bool true), body) in
  let body =
    match init with
    | Some init -> Ast.Block [init; body]
    | None -> body in
  (body, tokens)

and parse_function kind = function
  | { kind = Identifier name } :: tokens ->
      let tokens = consume Left_paren ("Expect '(' after " ^ kind ^ " name.") tokens in
      let rec get_arguments args tokens =
        let arg, tokens =
          match tokens with
          | { kind = Identifier name } :: tokens ->
              (name, tokens)
          | token :: _ as tokens ->
              error token "Expect parameter name.";
              raise (Parse_error tokens)
          | [] -> raise (Failure "Unreachable!") in
        let args = arg :: args in
        match tokens with
        | { kind = Comma } :: tokens -> get_arguments args tokens
        | tokens -> (List.rev args, tokens) in
      let arguments, tokens =
        match tokens with
        | { kind = Right_paren } :: _ as tokens -> ([], tokens)
        | tokens -> get_arguments [] tokens in
      let tokens = consume Right_paren ("Expect ')' after parameters.") tokens in
      let tokens = consume Left_brace ("Expect '{' before " ^ kind ^ " body.") tokens in
      let body, tokens = block [] tokens in
      (Ast.Function (name, arguments, body), tokens)
  | token :: _ as tokens ->
      error token ("Expect " ^ kind ^ " name.");
      raise (Parse_error tokens)
  | [] -> raise (Failure "Unreachable!")

and parse_return line = function
  | { kind = Semicolon } :: tokens ->
      (Ast.Return (Ast.Literal Ast.Nil, line), tokens)
  | tokens ->
      let expr, tokens = expression tokens in
      let tokens = consume Semicolon "Expect ';' after return value." tokens in
      (Ast.Return (expr, line), tokens)

let parse tokens =
  let rec loop stmts = function
    | { kind = EOF } :: _ -> List.rev stmts
    | tokens ->
        match declaration tokens with
        | Some stmt, tokens -> loop (stmt :: stmts) tokens
        | None, tokens -> loop stmts tokens in
  loop [] tokens
