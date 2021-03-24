module Environment = Map.Make(String)

type t =
  | Env of Value.t Environment.t * t option

let env : t ref =
  let clock =
    Value.Callable (0, "clock", fun _ -> Value.Number (Sys.time ())) in
  ref (Env (Environment.singleton "clock" clock, None))

let push () =
  env := Env (Environment.empty, Some !env)

let pop () =
  env := match !env with Env (current, Some env) -> env | _ -> !env

let define name value = 
  let current, previous =
    match !env with
    | Env (current, previous) -> (current, previous) in
  env := Env (Environment.add name value current, previous)

let assign name value k =
  let rec loop = function
    | Env (current, previous) ->
        if Environment.mem name current
          then Env (Environment.add name value current, previous)
          else
            match previous with
            | Some env -> Env (current, Some (loop env))
            | None -> k () in
  env := loop !env

let get name k =
  let rec loop = function
    | Env (current, previous) ->
        match Environment.find_opt name current with
        | Some value -> value
        | None ->
            match previous with
            | Some env -> loop env
            | None -> k () in
  loop !env
