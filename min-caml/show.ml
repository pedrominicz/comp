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
      if Option.is_some !typ then failwith "unreachable";
      Printf.sprintf "Var 0"

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

let ident s = "\"" ^ s ^ "\""

let rec kNormal = function
  | KNormal.Unit -> "Unit"
  | KNormal.Int int -> "Int " ^ Int.to_string int
  | KNormal.Float float -> Printf.sprintf "Float %.01f" float
  | KNormal.Neg var -> Printf.sprintf "Neg \"%s\"" var
  | KNormal.Add (var1, var2) -> Printf.sprintf "Add \"%s\" \"%s\"" var1 var2
  | KNormal.Sub (var1, var2) -> Printf.sprintf "Sub \"%s\" \"%s\"" var1 var2
  | KNormal.FNeg var -> Printf.sprintf "FNeg \"%s\"" var
  | KNormal.FAdd (var1, var2) -> Printf.sprintf "FAdd \"%s\" \"%s\"" var1 var2
  | KNormal.FSub (var1, var2) -> Printf.sprintf "FSub \"%s\" \"%s\"" var1 var2
  | KNormal.FMul (var1, var2) -> Printf.sprintf "FMul \"%s\" \"%s\"" var1 var2
  | KNormal.FDiv (var1, var2) -> Printf.sprintf "FDiv \"%s\" \"%s\"" var1 var2
  | KNormal.IfEq (var1, var2, then_branch, else_branch) -> Printf.sprintf "IfEq \"%s\" \"%s\" (%s) (%s)" var1 var2 (kNormal then_branch) (kNormal else_branch)
  | KNormal.IfLE (var1, var2, then_branch, else_branch) -> Printf.sprintf "IfLE \"%s\" \"%s\" (%s) (%s)" var1 var2 (kNormal then_branch) (kNormal else_branch)
  | KNormal.Let ((ident, _), exp1, exp2) -> Printf.sprintf "Let \"%s\" (%s) (%s)" ident (kNormal exp1) (kNormal exp2)
  | KNormal.Var ident -> Printf.sprintf "Var \"%s\"" ident
  | KNormal.LetRec ({ name = (name, _); args; body }, exp) ->
      let args = list (List.map ident (List.map fst args)) in
      Printf.sprintf "LetRec \"%s\" %s (%s) (%s)" name args (kNormal body) (kNormal exp)
  | KNormal.App (func, args) ->
      let args = list (List.map ident args) in
      Printf.sprintf "App \"%s\" %s" func args
  | KNormal.Tuple vars ->
      let vars = list (List.map ident vars) in
      Printf.sprintf "Tuple %s" vars
  | KNormal.LetTuple (items, tuple, exp) ->
      let items = list (List.map ident (List.map fst items)) in
      Printf.sprintf "LetTuple %s \"%s\" (%s)" items tuple (kNormal exp)
  | KNormal.Get (var1, var2) -> Printf.sprintf "Get \"%s\" \"%s\"" var1 var2
  | KNormal.Put (var1, var2, var3) -> Printf.sprintf "Put \"%s\" \"%s\" \"%s\"" var1 var2 var3
  | KNormal.ExtArray name -> Printf.sprintf "ExtArray \"%s\"" name
  | KNormal.ExtFunApp (func, args) ->
      let args = list (List.map ident args) in
      Printf.sprintf "ExtFunApp \"%s\" %s" func args
