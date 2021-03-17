type t = { kind : TokenKind.t; line : int }

let to_string { kind } = TokenKind.to_string kind

let make kind line = { kind = kind; line = line }
