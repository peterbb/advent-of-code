open Core

let day = 4

let example_input_1 = 
  {|
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
  |}

type card = { id: int; winning_numbers: int list; numbers_i_have: int list }

let tuple_of_list = function
  | [x; y] -> (x, y)
  | xs -> failwith (sprintf "list with %d elements cant be converted to tuple" (List.length xs))


let parse input =
  let parse_card_id card =
    String.chop_prefix_exn ~prefix:"Card " card
    |> String.strip
    |> int_of_string
  in
  let parse_numbers card_details = 
    card_details
    |> String.split ~on:'|'
    |> List.map ~f:String.strip
    |> List.map ~f:(fun numbers ->
      numbers
      |> String.split ~on:' '
      |> List.map ~f:String.strip
      |> List.filter ~f:(String.(<>) "")
      |> List.map ~f:int_of_string
    )
    |> tuple_of_list
  in
  let parse_card line = 
    let header, body = line |> String.split ~on:':' |> tuple_of_list in
    let id = parse_card_id header in
    let winning_numbers, numbers_i_have = parse_numbers body in
    { id; winning_numbers; numbers_i_have }
  in
  input
  |> Stringlib.nonblank_lines
  |> List.map ~f:parse_card

let wins { id = _; winning_numbers; numbers_i_have } =
  numbers_i_have
  |> List.count ~f:(List.mem ~equal:(=) winning_numbers)

let points card =
  match wins card with
  | 0 -> 0
  | winning_numbers -> Int.pow 2 (winning_numbers - 1)

let part1 input = 
  parse input
  |> List.sum (module Int) ~f:points
  |> sprintf "%d"

let part2 input =
  let cards: (int, int, _) Map.t = 
    parse input 
    |> Map.of_list_with_key_or_error (module Int) ~get_key:(fun card -> card.id)
    |> Or_error.ok_exn
    |> Map.map ~f:wins
  in
  let collect_prizes (card_id: int) =
    let w = Map.find_exn cards card_id in
    List.range ~start:`inclusive ~stop:`exclusive (card_id + 1) (card_id + 1 + w)
  in
  let rec collect_cards (hand: int list) (number_of_copies: int) = 
    if List.is_empty hand then
      number_of_copies
    else begin
      let number_of_copies = number_of_copies + List.length hand in
      let new_hand =
        hand
        |> List.concat_map ~f:collect_prizes
      in
      collect_cards new_hand number_of_copies
    end
  in
  collect_cards (Map.keys cards) 0
  |> sprintf "%d"