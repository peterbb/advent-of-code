open Core

let solutions: (int, string * string, _) Map.t =
  [ ]
  |> Map.of_alist_or_error (module Int)
  |> Or_error.ok_exn