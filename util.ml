let rec span p = function
  | hd :: tl when p hd ->
      let l1, l2 = span p tl in
      (hd :: l1, l2)
  | l -> ([], l)

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alphanum c = is_alpha c || is_digit c

let string_of_char_list l =
    let buffer = Buffer.create 8 in
    List.iter (Buffer.add_char buffer) l;
    Buffer.contents buffer

let char_list_of_string str =
  List.init (String.length str) (String.get str)

let safe_hd l = try Some (List.hd l) with Failure _ -> None

let float_to_string num =
  let str = Bytes.of_string (Printf.sprintf "%.15f" num) in
  let index = ref (Bytes.length str - 1) in
  while Bytes.get str !index == '0' do
    Bytes.set str !index '\x00';
    index := !index - 1
  done;
  if Bytes.get str !index == '.' then Bytes.set str !index '\x00';
  Bytes.to_string str
