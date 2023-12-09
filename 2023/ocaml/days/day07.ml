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
  | [_, 5] -> `five_of_a_kind
  | [_, 4; _, 1] -> `four_of_a_kind
  | [_, 3; _, 2] -> `full_house
  | [_, 3; _, 1; _, 1] -> `three_of_a_kind
  | [_, 2; _, 2; _, 1] -> `two_pair
  | [_, 2; _, 1; _, 1; _, 1] -> `one_pair
  | [_, 1; _, 1; _, 1; _, 1; _, 1] -> `high_card
  | s -> 
    failwith (
      sprintf "invalid hand %s : %s"
        (String.of_list hand)
        (s |> List.map ~f:(fun (a, b) -> sprintf "%c:%d" a b) |> String.concat ~sep:", ")
    )

let type_of_hand_with_joker hand =
  let rec summary = function
    | [] -> []
    | x::xs ->
      let same x' = Char.(x = x') in
      (x, 1 + List.count ~f:same xs) :: summary (List.filter ~f:(Fn.non same) xs)
  in
  let compare (x, c) (x', c') =
    if c = c' then
      if Char.(x = 'J') then -1
      else if Char.(x' = 'J') then 1
      else 0
    else
      compare c' c
  in
  let summary = summary hand |> List.sort ~compare in
  (* printf "hand %s: %s\n"
    (String.of_list hand)
    (summary |> List.map ~f:(fun (a, b) -> sprintf "%d '%c'" b a) |> String.concat ~sep:";  "); *)
  match summary with
  | [_, 5] -> `five_of_a_kind

  | ['J', 4; _, 1] -> `five_of_a_kind
  | [_, 4; 'J', 1] -> `five_of_a_kind
  | [_, 4; _, 1] -> `four_of_a_kind

  | ['J', 3; _, 2] -> `five_of_a_kind
  | [_, 3; 'J', 2] -> `five_of_a_kind
  | [_, 3; _, 2] -> `full_house

  | ['J', 3; _, 1; _, 1] -> `four_of_a_kind
  | [_, 3; 'J', 1; _, 1] -> `four_of_a_kind
  | [_, 3; _, 1; _, 1] -> `three_of_a_kind

  | ['J', 2; _, 2; _, 1] -> `four_of_a_kind
  | [_, 2; _, 2; 'J', 1] -> `full_house
  | [_, 2; _, 2; _, 1] -> `two_pair

  | ['J', 2; _, 1; _, 1; _, 1] -> `three_of_a_kind
  | [_, 2; 'J', 1; _, 1; _, 1] -> `three_of_a_kind
  | [_, 2; _, 1; _, 1; _, 1] -> `one_pair

  | ['J', 1; _, 1; _, 1; _, 1; _, 1] -> `one_pair
  | [_, 1; _, 1; _, 1; _, 1; _, 1] -> `high_card

  | s -> 
    failwith (
      sprintf "invalid hand %s : %s"
        (String.of_list hand)
        (s |> List.map ~f:(fun (a, b) -> sprintf "%c:%d" a b) |> String.concat ~sep:", ")
    )

let rank_of_type = function
  | `five_of_a_kind -> 6
  | `four_of_a_kind -> 5
  | `full_house -> 4
  | `three_of_a_kind -> 3
  | `two_pair -> 2
  | `one_pair -> 1
  | `high_card -> 0

let string_of_type = function
  | `five_of_a_kind -> "five of a kind"
  | `four_of_a_kind -> "four of a kind"
  | `full_house -> "full house"
  | `three_of_a_kind -> "three of a kind"
  | `two_pair -> "two pair"
  | `one_pair -> "one pair"
  | `high_card -> "high card"

let value_of_card = function
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

let value_of_card_with_joker = function
  | 'A' -> 12
  | 'K' -> 11
  | 'Q' -> 10
  | 'T' -> 9
  | '9' -> 8
  | '8' -> 7
  | '7' -> 6
  | '6' -> 5
  | '5' -> 4
  | '4' -> 3
  | '3' -> 2
  | '2' -> 1
  | 'J' -> 0
  | _ -> failwith "invalid rank"

let compare_card ~value_of_card x y =
  compare (value_of_card x) (value_of_card y)

let rec lex_compare ~value_of_card (xs: char list) (ys: char list): int =
  match xs, ys with
  | [], [] -> 0
  | x::xs, y::ys ->
    begin match compare (value_of_card x) (value_of_card y) with
    | 0 -> lex_compare ~value_of_card xs ys
    | c -> c
    end
  | _ -> failwith "nope"

let compare_hand (a : char list) (b : char list) : int =
  let x = type_of_hand a |> rank_of_type in
  let y = type_of_hand b |> rank_of_type in
  if x = y then
    lex_compare ~value_of_card:value_of_card a b
  else
    compare x y

let compare_hand_with_joker (a : char list) (b : char list) : int =
  let x = type_of_hand_with_joker a |> rank_of_type in
  let y = type_of_hand_with_joker b |> rank_of_type in
  if x = y then
    lex_compare ~value_of_card:value_of_card_with_joker a b
  else
    compare x y

let exec ~compare input =
  parse input
  |> List.sort ~compare:(fun (h, _) (h', _) -> compare h h')
  (* |> List.map ~f:(fun ((h, _) as x) ->
    printf "%s: %s\n" (String.of_list h) (type_of_hand_with_joker h |> string_of_type); 
    x) *)
  (* |> List.map ~f:(fun ((h, bid) as x) -> printf "%s %d\n" (String.of_list h) bid; x) *)
  |> List.mapi ~f:(fun rank (_, bid) -> bid * (rank + 1))
  |> List.sum (module Int) ~f:Fn.id
  |> sprintf "%d"

let part1 input =
  exec ~compare:compare_hand input

let part2 input =
  exec ~compare:compare_hand_with_joker input