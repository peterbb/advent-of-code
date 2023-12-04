open Core

let day = 2

let example_input_1 =
{|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|}

type colorcount = { red: int; green: int; blue: int }
type 'a game = { game_id: int; rounds: 'a }

module Colorcount = struct
  type t = colorcount
  let zero = { red = 0; green = 0; blue = 0 }
  let (+) x y =
    { red = x.red + y.red
    ; green = x.green + y.green
    ; blue = x.blue + y.blue
    }
end

let parse =
  let rec parse input : colorcount list game list =
    input
    |> Stringlib.nonblank_lines
    |> List.map ~f:parse_game
  and parse_game input : colorcount list game =
    match String.split ~on:':' input with
    | [header; body] ->
        let game_id = parse_header header in
        let rounds = String.split ~on:';' body |> List.map ~f:parse_round in
        { game_id; rounds }
    | _ -> failwith "malformed input"
  and parse_header input : int =
    input |> String.chop_prefix_exn ~prefix:"Game " |> Int.of_string
  and parse_round input : colorcount =
    input
    |> String.strip
    |> String.split ~on:','
    |> List.map ~f:parse_color_count
    |> List.sum (module Colorcount) ~f:Fn.id
  and parse_color_count input : colorcount =
    match input |> String.strip |> String.split ~on:' ' with
    | [n; "red"] -> { red = (Int.of_string n); green = 0; blue = 0 }
    | [n; "green"] -> { red = 0; green = (Int.of_string n); blue = 0 }
    | [n; "blue"] -> { red = 0; green = 0; blue = (Int.of_string n) }
    | _ -> failwith "malformed color"
  in
  parse

let required_colors_of_rounds rounds =
  let max_of color = 
    rounds
    |> List.map ~f:color
    |> List.max_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  { red = max_of (fun x -> x.red)
  ; green = max_of (fun x -> x.green)
  ; blue = max_of (fun x -> x.blue)
  }

  let transform_to_required_colors game =
    { game with rounds = required_colors_of_rounds game.rounds }

let power game = game.red * game.green * game.blue

let part1_value game =
  if game.rounds.red <= 12 && game.rounds.green <= 13 && game.rounds.blue <= 14 then
    game.game_id
  else
    0

let part2_value game =
  power game.rounds

let exec ~game_value input =
  input 
  |> parse
  |> List.map ~f:transform_to_required_colors
  |> List.sum (module Int) ~f:game_value
  |> sprintf "%d"

let part1 input = 
  exec ~game_value:part1_value input

let part2 input = 
  exec ~game_value:part2_value input
