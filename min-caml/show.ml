let list = function
  | [] -> "[]"
  | hd :: tl ->
      let buf = Buffer.create 1024 in
      Buffer.add_string buf "[";
      Buffer.add_string buf hd;
      let it elem =
        Buffer.add_string buf ", ";
        Buffer.add_string buf elem in
      List.iter it tl;
      Buffer.add_string buf "]";
      Bytes.to_string (Buffer.to_bytes buf)

let show_var var = if String.get var 0 <> 'T' then var else ""

let rec show_type = function
  | Type.Unit -> "Unit"
  | Type.Bool -> "Bool"
  | Type.Int -> "Int"
  | Type.Float -> "Float"
  | Type.Fun (args, ret) ->
      let args = List.map show_type args in
      Printf.sprintf "Fun %s (%s)" (list args) (show_type ret)
  | Type.Tuple (types) ->
      let types = List.map show_type types in
      Printf.sprintf "Tuple (%s)" (list types)
  | Type.Array typ -> Printf.sprintf "Array (%s)" (show_type typ)
  | Type.Var typ ->
      let show_some typ =
        Printf.sprintf "Just (%s)" (show_type typ) in
      let typ = Option.fold ~none:"Nothing" ~some:show_some !typ in
      Printf.sprintf "Var (%s)" typ

let pair (ident, typ) =
  Printf.sprintf "(\"%s\", %s)" (show_var ident) (show_type typ)

let rec syntax = function
  | Syntax.Unit -> "Unit"
  | Syntax.Bool bool -> if bool then "Bool True" else "Bool False"
  | Syntax.Int int -> "Int " ^ Int.to_string int
  | Syntax.Float float -> Printf.sprintf "Float %.01f" float
  | Syntax.Not exp -> Printf.sprintf "Not (%s)" (syntax exp)
  | Syntax.Neg exp -> Printf.sprintf "Neg (%s)" (syntax exp)
  | Syntax.Add (exp1, exp2) -> Printf.sprintf "Add (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.Sub (exp1, exp2) -> Printf.sprintf "Sub (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.FNeg exp -> Printf.sprintf "FNeg (%s)" (syntax exp)
  | Syntax.FAdd (exp1, exp2) -> Printf.sprintf "FAdd (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.FSub (exp1, exp2) -> Printf.sprintf "FSub (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.FMul (exp1, exp2) -> Printf.sprintf "FMul (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.FDiv (exp1, exp2) -> Printf.sprintf "FDiv (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.Eq (exp1, exp2) -> Printf.sprintf "Eq (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.LE (exp1, exp2) -> Printf.sprintf "LE (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.If (cond, then_branch, else_branch) -> Printf.sprintf "If (%s) (%s) (%s)" (syntax cond) (syntax then_branch) (syntax else_branch)
  | Syntax.Let ((ident, typ), exp1, exp2) ->
      let exp1 = syntax exp1 in
      let exp2 = syntax exp2 in
      let ident = show_var ident in
      let typ = show_type typ in
      Printf.sprintf "Let \"%s\" (%s) (%s) (%s)" ident typ exp1 exp2
  | Syntax.Var var -> Printf.sprintf "Var \"%s\"" (show_var var)
  | Syntax.LetRec ({ name = (name, typ); args; body }, exp) ->
      let name = show_var name in
      let typ = show_type typ in
      let args = List.map pair args in
      let args = list args in
      let body = syntax body in
      let exp = syntax exp in
      Printf.sprintf "LetRec \"%s\" (%s) %s (%s) (%s)" name typ args body exp
  | Syntax.App (func, args) ->
      let args = List.map syntax args in
      Printf.sprintf "App (%s) (%s)" (syntax func) (list args)
  | Syntax.Tuple exps ->
      let exps = List.map syntax exps in
      Printf.sprintf "Tuple %s" (list exps)
  | Syntax.LetTuple (idents, exp1, exp2) ->
      let idents = List.map pair idents in
      Printf.sprintf "LetTuple %s (%s) (%s)" (list idents) (syntax exp1) (syntax exp2)
  | Syntax.Array (exp1, exp2) -> Printf.sprintf "Array (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.Get (exp1, exp2) -> Printf.sprintf "Get (%s) (%s)" (syntax exp1) (syntax exp2)
  | Syntax.Put (exp1, exp2, exp3) -> Printf.sprintf "Put (%s) (%s) (%s)" (syntax exp1) (syntax exp2) (syntax exp3)
