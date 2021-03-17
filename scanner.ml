open TokenKind

let line = ref 1

let make kind = Token.make kind !line

let is_keyword_end = function
  | c :: _ when Util.is_alphanum c -> false
  | _ -> true

let rec loop tokens = function
  (* End of file. *)
  | [] -> List.rev (make EOF :: tokens)
  (* Whitespace. *)
  | ' ' :: tl | '\r' :: tl | '\t' :: tl -> loop tokens tl
  (* Newline. *)
  | '\n' :: tl ->
      line := !line + 1;
      loop tokens tl
  (* Comments. *)
  | '/' :: '/' :: tl ->
      let _, tl = Util.span (fun c -> c != '\n') tl in
      loop tokens tl
  (* Single-character tokens. *)
  | '(' :: tl -> loop (make LeftParen :: tokens) tl
  | ')' :: tl -> loop (make RightParen :: tokens) tl
  | '{' :: tl -> loop (make LeftBrace :: tokens) tl
  | '}' :: tl -> loop (make RightBrace :: tokens) tl
  | ',' :: tl -> loop (make Comma :: tokens) tl
  | '.' :: tl -> loop (make Dot :: tokens) tl
  | '-' :: tl -> loop (make Minus :: tokens) tl
  | '+' :: tl -> loop (make Plus :: tokens) tl
  | ';' :: tl -> loop (make Semicolon :: tokens) tl
  | '/' :: tl -> loop (make Slash :: tokens) tl
  | '*' :: tl -> loop (make Star :: tokens) tl
  (* One or two character tokens. *)
  | '!' :: '=' :: tl -> loop (make BangEqual :: tokens) tl
  | '!' :: tl ->        loop (make Bang :: tokens) tl
  | '=' :: '=' :: tl -> loop (make EqualEqual :: tokens) tl
  | '=' :: tl ->        loop (make Equal :: tokens) tl
  | '>' :: '=' :: tl -> loop (make GreaterEqual :: tokens) tl
  | '>' :: tl ->        loop (make Greater :: tokens) tl
  | '<' :: '=' :: tl -> loop (make LessEqual :: tokens) tl
  | '<' :: tl ->        loop (make Less :: tokens) tl
  (* Keywords. *)
  | 'a' :: 'n' :: 'd' :: tl when is_keyword_end tl ->
      loop (make And :: tokens) tl
  | 'c' :: 'l' :: 'a' :: 's' :: 's' :: tl when is_keyword_end tl ->
      loop (make Class :: tokens) tl
  | 'e' :: 'l' :: 's' :: 'e' :: tl when is_keyword_end tl ->
      loop (make Else :: tokens) tl
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tl when is_keyword_end tl ->
      loop (make False :: tokens) tl
  | 'f' :: 'o' :: 'r' :: tl when is_keyword_end tl ->
      loop (make For :: tokens) tl
  | 'f' :: 'u' :: 'n' :: tl when is_keyword_end tl ->
      loop (make Fun :: tokens) tl
  | 'i' :: 'f' :: tl when is_keyword_end tl ->
      loop (make If :: tokens) tl
  | 'n' :: 'i' :: 'l' :: tl when is_keyword_end tl ->
      loop (make Nil :: tokens) tl
  | 'o' :: 'r' :: tl when is_keyword_end tl ->
      loop (make Or :: tokens) tl
  | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tl when is_keyword_end tl ->
      loop (make Print :: tokens) tl
  | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tl when is_keyword_end tl ->
      loop (make Return :: tokens) tl
  | 's' :: 'u' :: 'p' :: 'e' :: 'r' :: tl when is_keyword_end tl ->
      loop (make Super :: tokens) tl
  | 't' :: 'h' :: 'i' :: 's' :: tl when is_keyword_end tl ->
      loop (make This :: tokens) tl
  | 't' :: 'r' :: 'u' :: 'e' :: tl when is_keyword_end tl ->
      loop (make True :: tokens) tl
  | 'v' :: 'a' :: 'r' :: tl when is_keyword_end tl ->
      loop (make Var :: tokens) tl
  | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: tl when is_keyword_end tl ->
      loop (make While :: tokens) tl
  (* Literals. *)
  | c :: _ as tl when Util.is_alpha c ->
      let identifier, tl = Util.span Util.is_alphanum tl in
      let identifier = Util.string_of_char_list identifier in
      loop (make (Identifier identifier) :: tokens) tl
  | '"' :: tl ->
      let str, tl = Util.span (fun c -> c != '"') tl in
      let str = Util.string_of_char_list str in
      (match tl with
      | _ :: tl -> loop (make (String str) :: tokens) tl
      | [] ->
          Error.error !line "Unterminated string.";
          loop (make (String str) :: tokens) tl)
  | c :: _ as tl when Util.is_digit c ->
      let num, tl = Util.span (fun c -> Util.is_digit c || c == '.') tl in
      (try
        let num = Float.of_string (Util.string_of_char_list num) in
        loop (make (Number num) :: tokens) tl
      with Failure _ ->
        Error.error !line "Expected float.";
        loop tokens tl)
  | c :: tl ->
      Error.error !line "Unexpected character.";
      loop tokens tl

let scan_tokens str =
  loop [] (Util.char_list_of_string str)
