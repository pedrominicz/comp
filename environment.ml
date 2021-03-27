type t =
  | Env of (string, Value.t) Hashtbl.t * t option

let empty : t = Env (Hashtbl.create 32, None)

let env : t ref = ref empty

let push () =
  env := Env (Hashtbl.create 32, Some !env)

let pop () =
  env := match !env with Env (current, Some env) -> env | _ -> !env

let define name value = 
  let current =
    match !env with
    | Env (current, previous) -> current in
  Hashtbl.add current name value

let assign name value k =
  let rec loop = function
    | Env (current, previous) ->
        if Hashtbl.mem current name
          then Hashtbl.add current name value
          else
            match previous with
            | Some env -> loop env
            | None -> k () in
  loop !env

let get name k =
  let rec loop = function
    | Env (current, previous) ->
        match Hashtbl.find_opt current name with
        | Some value -> value
        | None ->
            match previous with
            | Some env -> loop env
            | None -> k () in
  loop !env
