open Core
open Advent_of_code

module IntMap = Map.Make(Int)

let input_crabs_per_position =
    read_lines ()
    |> List.hd_exn
    |> String.split ~on:','
    |> List.map ~f:(fun n -> int_of_string n, 1)
    |> IntMap.of_alist_reduce ~f:(+)

let min_pos = 0
let max_pos = IntMap.max_elt_exn input_crabs_per_position |> fst
let positions = 
    Sequence.range ~start:`inclusive min_pos ~stop:`inclusive max_pos
    |> Sequence.to_list

let size = List.length positions
let distances = List.mapi ~f:(fun i _ -> i + 1) (List.tl_exn positions)
let distances = List.rev distances @ [0] @ distances

let crabs_per_position =
    let f ~key:_ = function
        | `Both (x, y) -> Some (x + y)
        | `Right x | `Left x -> Some x
    in
    positions
    |> List.map ~f:(fun pos -> pos, 0)
    |> IntMap.of_alist_exn
    |> IntMap.merge ~f input_crabs_per_position
    |> IntMap.to_alist ~key_order:`Increasing 
    |> List.map ~f:snd


let all_costs cost =
    let f i n = 
        distances
        |> Fn.flip List.drop (size - i - 1)
        |> Fn.flip List.take size
        |> List.map ~f:(fun x -> cost x * n)
    in
    crabs_per_position
    |> List.mapi ~f

let solve cost = 
    all_costs cost
    |> List.transpose_exn 
    |> List.map ~f:(List.sum (module Int) ~f:Fn.id)
    |> List.min_elt ~compare:Int.compare
    |> (fun i -> Option.value_exn i)
    |> printf "%d\n"



let () = solve Fn.id

(*
 *   0 1 2 3 4 5 6
 * + 6 5 4 3 2 1 0
 * =
 *   6 6 6 6 6 6 6
 * 
 *   (6 * (6 + 1)) / 2
 *)
let () = solve (fun distance -> (distance * (distance + 1)) / 2)

