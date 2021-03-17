let had_error = ref false

let report line where message =
  Format.eprintf "[line %d] Error%s: %s\n" line where message

let error line message =
  report line "" message

let run s =
  Scanner.source := s;
  let tokens = Scanner.scan_tokens () in
  List.iter (fun token -> print_endline (Token.to_string token)) tokens

let rec run_prompt () =
  print_string "> ";
  flush stdout;
  let s = try Some(input_line stdin) with End_of_file -> None in
  match s with
  | None -> ()
  | Some s ->
      run (Bytes.of_string s);
      had_error := false;
      run_prompt ()

let run_file file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  run s;
  if !had_error then exit 1 else ()

let () =
  match Array.length Sys.argv with
  | 1 -> run_prompt ()
  | 2 -> run_file Sys.argv.(1)
  | _ ->
      Format.eprintf "usage: %s [script]\n" Sys.argv.(0);
      exit 1
