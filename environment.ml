module Environment = Map.Make(String)

type t = Value.t Environment.t

let env : t ref = ref Environment.empty

let define name value = Environment.update name (fun _ -> Some value) !env

let get name k =
  match Environment.find_opt name !env with
  | Some value -> value
  | None -> k ()
