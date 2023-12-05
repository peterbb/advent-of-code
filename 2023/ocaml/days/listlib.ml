open Core

let split ~f xs =
  let rec loop current acc xs =
    match xs with
    | x :: xs when f x -> loop [] (List.rev current :: acc) xs
    | x :: xs -> loop (x :: current) acc xs
    | [] -> List.rev current :: acc
  in
  loop [] [] xs