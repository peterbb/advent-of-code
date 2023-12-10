open Core

let day = 8

let example_input_1 = {|
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|}

let example_input_2 = {|
  LLR

  AAA = (BBB, BBB)
  BBB = (AAA, ZZZ)
  ZZZ = (ZZZ, ZZZ)
|}

let example_input_3 = {|
  LR

  11A = (11B, XXX)
  11B = (XXX, 11Z)
  11Z = (11B, XXX)
  22A = (22B, XXX)
  22B = (22C, 22C)
  22C = (22Z, 22Z)
  22Z = (22B, 22B)
  XXX = (XXX, XXX)
|}

let regex = 
  let open Re in
  rep1 (alt [alpha; digit]) |> compile

let parse input =
  let parse_node line =
    match Re.matches regex line with
    | [x; y; z] -> x, (y, z)
    | xs -> failwith (sprintf "regex not three matches, but '%s'" (String.concat ~sep:"|" xs))
  in
  match Stringlib.nonblank_lines input with
  | path :: map ->
    path, Array.of_list_map ~f:parse_node map
  | _ -> failwith "not enough lines"


let solve input is_start_node =
  let path, nodes = parse input in

  let leftwards =
    nodes
    |> Array.map ~f:(fun (node, (left, _)) -> node, left)
    |> Array.to_sequence
    |> Map.of_sequence_exn  (module String)
  in

  let rightwards =
    nodes
    |> Array.map ~f:(fun (node, (_, right)) -> node, right)
    |> Array.to_sequence
    |> Map.of_sequence_exn  (module String)
  in

  let path =
    path 
    |> String.to_array 
    |> Array.map ~f:(function 'L' -> leftwards | 'R' -> rightwards | _ -> failwith "err")
  in

  let path_len = Array.length path in

  let steps_until_terminal start_node =
    let rec loop count current_node =
      if Char.(current_node.[2] = 'Z') then
        count |> Z.of_int
      else
        loop (count + 1) (Map.find_exn path.(count % path_len) current_node)
    in
    loop 0 start_node
  in

  nodes 
  |> Array.filter ~f:is_start_node
  |> Array.map ~f:(fun (node, _) -> steps_until_terminal node)
  |> Array.fold ~init:Z.one ~f:Z.lcm
  |> Z.to_string


let part1 input = solve input (fun (node, _) -> String.(node = "AAA"))
let part2 input = solve input (fun (node, _) -> Char.(node.[2] = 'A'))