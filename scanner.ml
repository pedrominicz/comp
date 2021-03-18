open TokenKind

let line = ref 1

let make kind = Token.make kind !line

module Map = Map.Make(String)

let keywords = Map.of_seq (List.to_seq [
  ("and", And);
  ("class", Class);
  ("else", Else);
  ("false", False);
  ("for", For);
  ("fun", Fun);
  ("if", If);
  ("nil", Nil);
  ("or", Or);
  ("print", Print);
  ("return", Return);
  ("super", Super);
  ("this", This);
  ("true", True);
  ("var", Var);
  ("while", While)])

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
  (* Keywords and literals. *)
  | c :: _ as tl when Util.is_alpha c ->
      let identifier, tl = Util.span Util.is_alphanum tl in
      let identifier = Util.string_of_char_list identifier in
      let token =
        match Map.find_opt identifier keywords with
        | Some keyword -> make keyword
        | None -> make (Identifier identifier) in
      loop (token :: tokens) tl
  | '"' :: tl ->
      let str, tl = Util.span (fun c -> c != '"') tl in
      let str = Util.string_of_char_list str in
      (match tl with
      | _ :: tl -> loop (make (String str) :: tokens) tl
      | [] ->
          Error.error !line "Unterminated string.";
          loop (make (String str) :: tokens) [])
  | c :: _ as tl when Util.is_digit c ->
      let num, tl = Util.span Util.is_digit tl in
      let num, tl =
        match tl with
        | '.' :: c :: _ as tl when Util.is_digit c ->
            let num', tl = Util.span Util.is_digit (List.tl tl) in
            (List.concat [num; ['.']; num'], tl)
        | _ -> (num, tl) in
      let num = Float.of_string (Util.string_of_char_list num) in
      loop (make (Number num) :: tokens) tl
  | c :: tl ->
      Error.error !line "Unexpected character.";
      loop tokens tl

let scan_tokens str =
  loop [] (Util.char_list_of_string str)
