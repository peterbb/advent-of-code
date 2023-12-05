open Core

let day = 5

let example_input_1 =
  {|
    seeds: 79 14 55 13

    seed-to-soil map:
    50 98 2
    52 50 48

    soil-to-fertilizer map:
    0 15 37
    37 52 2
    39 0 15

    fertilizer-to-water map:
    49 53 8
    0 11 42
    42 0 7
    57 7 4

    water-to-light map:
    88 18 7
    18 25 70

    light-to-temperature map:
    45 77 23
    81 45 19
    68 64 13

    temperature-to-humidity map:
    0 69 1
    1 0 69

    humidity-to-location map:
    60 56 37
    56 93 4
  |}


type range = 
  { from: int (* included *)
  ; to_: int (* excluded *)
  }

type range_mapping = { domain: range; adjustment: int }

type map =
  { source_category: string
  ; destination_category: string
  ; range_mappings: range_mapping list
  }

type input = { seeds: int list; maps: map list}

let parse input: input =
  let sections =
    input
    |> String.split ~on:'\n'
    |> List.map ~f:String.strip
    |> Listlib.split ~f:(String.(=) "")
    |> List.filter ~f:(Fn.non List.is_empty)
    |> List.rev
  in

  let parse_seeds: string list -> int list = function
  | [ line ] -> 
    line
    |> String.chop_prefix_exn ~prefix:"seeds: "
    |> String.split ~on:' '
    |> List.map ~f:int_of_string
  | _ -> failwith "wrong number of lines for seed-chunk"
  in

  let parse_header header : string * string = 
    header
    |> String.chop_suffix_exn ~suffix:" map:"
    |> String.split ~on:'-'
    |> function
    | [from; "to"; to_] -> from, to_
    | _ -> failwith "invalid map header"
  in

  let parse_mapping input : range_mapping =
    input
    |> String.split ~on:' '
    |> List.map ~f:int_of_string
    |> function
      | [dest_index; source_index; count] -> 
        { domain = { from = source_index; to_ = source_index + count }
        ; adjustment = -source_index + dest_index
        }
      | _ -> failwith "wrong number of elements in mapping"
  in

  let parse_map: string list -> map = function
  | header :: mappings ->
    let source_category, destination_category = parse_header header in
    let range_mappings = List.map ~f:parse_mapping mappings in
    { source_category; destination_category; range_mappings }
  | _ -> failwith "invalid mapping section"
  in

  match sections with
  | [] -> failwith "no seeds"
  | seeds :: mappings -> 
    let seeds = parse_seeds seeds in
    let maps = List.map ~f:parse_map mappings in
    { seeds; maps }

let is_nonempty_range { from; to_ } = from < to_

let range_intersect x y: range list = 
  [{ from = Int.max x.from y.from 
  ; to_ = Int.min x.to_ y.to_
  }]
  |> List.filter ~f:is_nonempty_range

let range_set_minus x y: range list =
  if y.to_ <= x.from then
    [x]
  else if x.to_ <= y.from then
    [x]
  else begin
    [ { from = x.from; to_ = y.from }
    ; { from = y.to_; to_ = x.to_ } ]
    |> List.filter ~f:is_nonempty_range
  end


let range_shift ~by {from; to_} =
  { from = from + by; to_ = to_ + by }

type apply_result = { converted_ranges: range list; nonconverted_ranges: range list }
let apply ~range_mapping:{domain; adjustment} (range: range) : apply_result =
  let intersection = range_intersect domain range in
  let outside = range_set_minus range domain in
  { converted_ranges = intersection |> List.map ~f:(range_shift ~by:adjustment)
  ; nonconverted_ranges = outside
  }

let rec apply' ~range_mappings range : range list = 
  match range_mappings with
  | [] -> [range]
  | range_mapping :: range_mappings ->
    let {converted_ranges; nonconverted_ranges} = apply ~range_mapping range in
    converted_ranges @ List.concat_map ~f:(apply' ~range_mappings) nonconverted_ranges

let apply'' ~range_mappings (ranges: range list): range list = 
  List.concat_map ~f:(apply' ~range_mappings) ranges

let rewrite_path = [ "seed"; "soil"; "fertilizer"; "water"; "light"; "temperature"; "humidity"; "location" ]

let lookup ~maps ~from ~to_ =
  let open String in
  List.find_exn ~f:(fun map -> from = map.source_category && to_ = map.destination_category) maps

let rewrite_ranges ~maps:(maps : map list) (ranges: range list): range list =
  let rec loop (ranges: range list) (path: string list) : range list = 
    match path with
    | from :: (to_ :: _ as rest) -> 
      let { range_mappings; _ } = lookup ~maps ~from ~to_ in
      loop (apply'' ~range_mappings ranges) rest
    | _ -> ranges
  in
  loop ranges rewrite_path


let exec input init_factory = 
  let {seeds; maps} = parse input in
  init_factory seeds
  |> rewrite_ranges ~maps
  |> List.map ~f:(fun {from; _} -> from)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> sprintf "%d"

let part1 input = 
  exec input (List.map ~f:(fun i -> { from = i; to_ = i + 1}))

let part2 input = 
  let rec init = function
    | [] -> []
    | start :: count :: rest ->
        { from = start; to_ = (start + count)} :: (init rest)
    | _  -> failwith "malformed seed line"
  in
  exec input init