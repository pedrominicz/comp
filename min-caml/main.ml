let rec main () =
  let str = try Some (input_line stdin) with End_of_file -> None in
  match str with
  | None -> ()
  | Some str ->
    let exp = Parser.exp Lexer.token (Lexing.from_string str) in
    print_endline (Show.syntax exp);
    main ()

let () = main ()
