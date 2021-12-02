open Core

let sliding_window = 
    let rec loop acc = function
    | x :: (y :: _ as rest) -> loop ((x, y) :: acc) rest
    | _ -> List.rev acc
    in
    loop []

let sliding_window_3 = 
    let rec loop acc = function
    | x :: (y :: z :: _ as rest) -> loop ((x, y, z) :: acc) rest
    | _ -> List.rev acc
    in
    loop []

let lines = In_channel.input_lines In_channel.stdin
    |> List.map ~f:int_of_string

let count_increase = List.count ~f:(fun (x, y) -> x < y)

let () = 
    lines
    |> sliding_window
    |> count_increase
    |> printf "part 1: %d\n"

let () =
    lines
    |> sliding_window_3
    |> List.map ~f:(fun (x, y, z) -> x + y + z)
    |> sliding_window
    |> count_increase
    |> printf "part 2: %d\n"


