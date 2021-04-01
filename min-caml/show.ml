let list = function
  | [] -> "[]"
  | hd :: tl ->
      let buf = Buffer.create 1024 in
      Buffer.add_string buf "[";
      Buffer.add_string buf hd;
      let it elem =
        Buffer.add_string buf "; ";
        Buffer.add_string buf elem in
      List.iter it tl;
      Buffer.add_string buf "]";
      Bytes.to_string (Buffer.to_bytes buf)

let rec syntax = function
  | Syntax.Unit -> "Unit"
  | Syntax.Bool bool -> "Bool " ^ Bool.to_string bool
  | Syntax.Int int -> "Int " ^ Int.to_string int
  | Syntax.Float float -> "Float " ^ Float.to_string float
  | Syntax.Not exp -> Printf.sprintf "Not (%s)" (syntax exp)
  | Syntax.Neg exp -> Printf.sprintf "Neg (%s)" (syntax exp)
  | Syntax.Add (exp1, exp2) -> Printf.sprintf "Add (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.Sub (exp1, exp2) -> Printf.sprintf "Sub (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.FNeg exp -> Printf.sprintf "FNeg (%s)" (syntax exp)
  | Syntax.FAdd (exp1, exp2) -> Printf.sprintf "FAdd (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.FSub (exp1, exp2) -> Printf.sprintf "FSub (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.FMul (exp1, exp2) -> Printf.sprintf "FMul (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.FDiv (exp1, exp2) -> Printf.sprintf "FDiv (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.Eq (exp1, exp2) -> Printf.sprintf "Eq (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.LE (exp1, exp2) -> Printf.sprintf "LE (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.If (cond, then_branch, else_branch) -> Printf.sprintf "If (%s, %s, %s)" (syntax cond) (syntax then_branch) (syntax else_branch)
  | Syntax.Let ((ident, _), exp1, exp2) -> Printf.sprintf "Let (%s, %s, %s)" ident (syntax exp1) (syntax exp2)
  | Syntax.Var var -> Printf.sprintf "Var \"%s\"" var
  | Syntax.LetRec ({ name = (name, _); args; body }, exp) ->
      let args = List.map fst args in
      Printf.sprintf "LetRec (%s, %s, %s, %s)" name (list args) (syntax body) (syntax exp)
  | Syntax.App (func, args) ->
      let args = List.map syntax args in
      Printf.sprintf "App (%s, %s)" (syntax func) (list args)
  | Syntax.Tuple exps ->
      let exps = List.map syntax exps in
      Printf.sprintf "Tuple %s" (list exps)
  | Syntax.LetTuple (idents, exp1, exp2) ->
      let idents = List.map fst idents in
      Printf.sprintf "LetTuple (%s, %s, %s)" (list idents) (syntax exp1) (syntax exp2)
  | Syntax.Array (exp1, exp2) -> Printf.sprintf "Array (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.Get (exp1, exp2) -> Printf.sprintf "Get (%s, %s)" (syntax exp1) (syntax exp2)
  | Syntax.Put (exp1, exp2, exp3) -> Printf.sprintf "Put (%s, %s, %s)" (syntax exp1) (syntax exp2) (syntax exp3)
