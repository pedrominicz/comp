let had_error = ref false

let report line where message =
  Format.eprintf "[line %d] Error%s: %s\n%!" line where message;
  had_error := true

let error line message =
  report line "" message
