let () =
  let s = input_line stdin in
  let _ = Parser.exp Lexer.token (Lexing.from_string s) in
  ()
