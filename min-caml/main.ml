let () =
  let str = input_line stdin in
  let exp = Parser.exp Lexer.token (Lexing.from_string str) in
  print_endline (Show.syntax exp)
