let had_error = ref false

let report line where msg =
  Format.eprintf "[line %d] Error%s: %s\n%!" line where msg;
  had_error := true

let error line msg =
  report line "" msg
