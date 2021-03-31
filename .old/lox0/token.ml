type t = { kind : Token_kind.t; line : int }

let to_string { kind } = Token_kind.to_string kind

let make kind line = { kind = kind; line = line }
