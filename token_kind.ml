type t =
  (* Single-character tokens. *)
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* One or two character tokens. *)
  | Bang
  | Bang_equal
  | Equal
  | Equal_equal
  | Greater
  | Greater_equal
  | Less
  | Less_equal
  (* Literals. *)
  | Identifier of string
  | String of string
  | Number of float
  (* Keywords. *)
  | And
  | Class
  | Else
  | False
  | For
  | Fun
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  (* End of file. *)
  | EOF

let to_string = function
  (* Single-character tokens. *)
  | Left_paren -> "("
  | Right_paren -> ")"
  | Left_brace -> "{"
  | Right_brace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Minus -> "-"
  | Plus -> "+"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  (* One or two character tokens. *)
  | Bang -> "!"
  | Bang_equal -> "!="
  | Equal -> "="
  | Equal_equal -> "=="
  | Greater -> ">"
  | Greater_equal -> ">="
  | Less -> "<"
  | Less_equal -> "<="
  (* Literals. *)
  | Identifier identifier -> identifier
  | String str -> "\"" ^ str ^ "\""
  | Number num -> Float.to_string num
  (* Keywords. *)
  | And -> "and"
  | Class -> "class"
  | Else -> "else"
  | False -> "false"
  | For -> "for"
  | Fun -> "fun"
  | If -> "if"
  | Nil -> "nil"
  | Or -> "or"
  | Print -> "print"
  | Return -> "return"
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
  (* End of file. *)
  | EOF -> "<eof>"
