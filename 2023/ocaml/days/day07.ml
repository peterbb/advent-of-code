open Core
let day = 7

let example_input_1 = {|
  32T3K 765
  T55J5 684
  KK677 28
  KTJJT 220
  QQQJA 483
|}

let tuple_of_list = function
  | [x; y] -> x, y
  | _ -> failwith "malformed tuple-as-list"

let parse input: (char list * int) list =
  let parse_hand line = 
    let hand, bid = 
      line
      |> String.split ~on:' '
      |> tuple_of_list
    in
    String.to_list hand, int_of_string bid
  in
  input 
  |> Stringlib.nonblank_lines 
  |> List.map ~f:parse_hand

let type_of_hand hand =
  let rec summary = function
    | [] -> []
    | x::xs ->
      let same x' = Char.(x = x') in
      (x, 1 + List.count ~f:same xs) :: summary (List.filter ~f:(Fn.non same) xs)
  in
  match summary hand |> List.sort ~compare:(fun (_, c) (_, c') -> compare c' c) with
  | [_, 5] -> `five_of_a_kind, 6
  | [_, 4; _, 1] -> `four_of_a_kind, 5
  | [_, 3; _, 2] -> `full_house, 4
  | [_, 3; _, 1; _, 1] -> `three_of_a_kind, 3
  | [_, 2; _, 2; _, 1] -> `two_pair, 2
  | [_, 2; _, 1; _, 1; _, 1] -> `one_pair, 1
  | [_, 1; _, 1; _, 1; _, 1; _, 1] -> `high_card, 0
  | s -> 
    failwith (
      sprintf "invalid hand %s : %s"
        (String.of_list hand)
        (s |> List.map ~f:(fun (a, b) -> sprintf "%c:%d" a b) |> String.concat ~sep:", ")
    )

let relative_rank = function
  | 'A' -> 12
  | 'K' -> 11
  | 'Q' -> 10
  | 'J' -> 9
  | 'T' -> 8
  | '9' -> 7
  | '8' -> 6
  | '7' -> 5
  | '6' -> 4
  | '5' -> 3
  | '4' -> 2
  | '3' -> 1
  | '2' -> 0
  | _ -> failwith "invalid rank"

let compare_card x y =
  compare (relative_rank x) (relative_rank y)

let rec lex_compare xs ys =
  match xs, ys with
  | [], [] -> 0
  | x::xs, y::ys ->
    begin match compare_card x y with
    | 0 -> lex_compare xs ys
    | c -> c
    end
  | _ -> failwith "nope"

let compare_hand (a : char list) (b : char list) : int =
  let _, x = type_of_hand a in
  let _, y = type_of_hand b in
  if x = y then
    lex_compare a b
  else
    compare x y

let part1 input =
  let input = parse input in
  let input: (char list * int) list = input |> List.sort ~compare:(fun (h, _) (h', _) -> compare_hand h h') in
  (* input |> List.iter ~f:(fun (h, bid) -> printf "%s %d\n" (String.of_list h) bid); *)
  input
  |> List.mapi ~f:(fun rank (_, bid) -> bid * (rank + 1))
  |> List.sum (module Int) ~f:Fn.id
  |> sprintf "%d"

let part2 _ = ""