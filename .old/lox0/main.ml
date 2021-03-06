let run str =
  let tokens = Scanner.scan_tokens str in
  let statements = Parser.parse tokens in
  if not !Error.had_error then Interpreter.interpret statements

let rec run_prompt () =
  print_string "> ";
  flush stdout;
  let str = try Some (input_line stdin) with End_of_file -> None in
  match str with
  | None -> print_newline ()
  | Some str ->
      run str;
      Error.had_error := false;
      Error.had_runtime_error := false;
      run_prompt ()

let run_file file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let str = really_input_string ic n in
  close_in ic;
  run str;
  if !Error.had_error || !Error.had_runtime_error then exit 1 else ()

let () =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ ->
      Printf.eprintf "usage: %s [script]\n" Sys.argv.(0);
      exit 1
