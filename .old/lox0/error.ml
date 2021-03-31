let had_error = ref false
let had_runtime_error = ref false

let report line where msg =
  Printf.eprintf "[line %d] Error%s: %s\n%!" line where msg;
  had_error := true

let error line msg =
  report line "" msg

let runtime_error line msg =
  Printf.eprintf "%s\n[line %d]\n%!" msg line;
  had_runtime_error := true
