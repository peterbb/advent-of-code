open Core

let find_first ~f s = 
  String.find  ~f s

let find_last ~f t  =
  match String.rfindi t ~f:(fun _ c -> f c) with
  | None -> None
  | Some i -> Some t.[i]

let substr_first_index line ~pattern =
  String.substr_index line ~pattern

let substr_last_index line ~pattern =
  String.substr_index (String.rev line) ~pattern:(String.rev pattern)


  let nonblank_lines input =
    input 
    |> String.split ~on:'\n' 
    |> List.map ~f:String.strip
    |> List.filter ~f:(String.(<>) "")
