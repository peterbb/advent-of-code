open Core
open Printf

let day = 6

let example_input_1 =
  {|
    Time:      7  15   30
    Distance:  9  40  200
  |}


module IntProduct = struct
  type t = int
  let zero = 1
  let (+) = ( * )
end

module Race = struct
  type t = { time_allowed: int; best_distance: int }
end

let parse parser input : Race.t list =
  let parse_line line: int list =
    line 
    |> String.split ~on:':'
    |> Fn.flip List.nth_exn 1
    |> String.split ~on:' '
    |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
    |> parser
  in
  input
  |> Stringlib.nonblank_lines 
  |> List.map ~f:parse_line
  |> function 
    | [ times_allowed; best_distances] ->
      List.map2_exn ~f:(fun time_allowed best_distance -> Race.{time_allowed; best_distance}) times_allowed best_distances
    | _ ->
      failwith "wrong input"

type t = { time_allowed: int; best_distance: int; possible_distances: int list }

let possible_travel_distances Race.{time_allowed; best_distance} =
  let distance_traveled button_press_time =
    let speed = button_press_time in
    let travel_time = time_allowed - button_press_time in
    speed * travel_time
  in
  let possible_distances = 
    List.range ~start:`inclusive ~stop:`inclusive 0 time_allowed
    |> List.map ~f:distance_traveled
  in
  { time_allowed; best_distance; possible_distances }


let solve races = 
  let possible_times = races |> List.map ~f:(fun Race.{time_allowed; best_distance} -> {time_allowed; best_distance; possible_distances = possible_travel_distances time_allowed}) in
  let acceptable_times = possible_times |> List.map ~f:(fun {best_distance; possible_distances; _} -> List.filter ~f:(fun possible_distance -> possible_distance > best_distance) possible_distances) in
  List.sum (module IntProduct) ~f:List.length acceptable_times
  |> sprintf "%d"

let parser_part1 (numbers: string list) : int list = 
  numbers |> List.map ~f:int_of_string

let parser_part2 (numbers: string list) : int list =
  [String.concat numbers |> int_of_string]

let part1 input =
  parse parser_part1 input
  |> solve

let part2 input =
  parse parser_part2 input
  |> solve