open Core

let day = 1

let example_input = 
"3   4
4   3
2   5
1   3
3   9
3   3"

module Parser = Day01_parser

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t\n\r") -> token buf
  | Plus ('0' .. '9') -> Day01_parser.NUM (Sedlexing.Utf8.lexeme buf |> Int.of_string)
  | eof -> Day01_parser.EOF
  | _ -> failwith "Unexpected character"

type input = int list * int list

let part1 (first_column, second_column) = 
  List.map2_exn
    (List.sort ~compare:Int.compare first_column)
    (List.sort ~compare:Int.compare second_column)
    ~f:Int.(-)
  |> List.sum (module Int) ~f:Int.abs
  |> Int.to_string

let part2 (first_column, second_column) = 
  let counts = List.map second_column ~f:(fun id -> Map.singleton (module Int) id 1)
    |> List.fold
        ~init:(Map.empty (module Int))
        ~f:(Map.merge ~f:(fun ~key:_ e ->
          match e with
          | `Left x | `Right x -> Some x
          | `Both (x, y) -> Some (x + y)
        ))
  in
  first_column
  |> List.sum (module Int)
    ~f:(fun id -> id * (Map.find counts id |> Option.value ~default:0))
  |> Int.to_string

