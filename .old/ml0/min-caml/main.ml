let rec main () =
  let str = try Some (input_line stdin) with End_of_file -> None in
  match str with
  | None -> ()
  | Some str ->
    let exp =
      Alpha.f
        (KNormal.f
          (Typing.f
            (Parser.exp Lexer.token (Lexing.from_string str)))) in
    print_endline (Show.kNormal exp);
    main ()

let () = main ()
