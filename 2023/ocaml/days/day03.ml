open Core

let day = 3

let example_input_1 = 
  {|
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
  |}

let coord_eq (i, j) (i', j') =
  i = i' && j = j'

let adjecent_indexes (i, j, j_end) =
  (i, j - 1) ::
  (i, j_end) ::
  (List.range ~start:`inclusive ~stop:`inclusive (j - 1) j_end
    |> List.concat_map ~f:(fun j -> [i - 1, j; i + 1, j]))

let parse input = 
  let engine =
    input
    |> Stringlib.nonblank_lines
    |> Array.of_list
  in
  let extract_numbers i line = 
    let rec init j =
      if j >= String.length line then
        []
      else if Char.is_digit line.[j] then
        collect_number j (j + 1)
      else
        init (j + 1)
    and collect_number start j = 
        if j >= String.length line then
          [(i, start, j)]
        else if Char.is_digit line.[j] then
          collect_number start (j + 1)
        else
          (i, start, j) :: init j
        in
      init 0 |> Array.of_list
  in
  let valid_index (i, j) =
    0 <= i && 0 <= j &&
    i < Array.length engine &&
    j < String.length engine.(0)
  in
  let is_symbol (i, j) = 
    let c = engine.(i).[j] in
    Char.(c <> '.') && not (Char.is_digit c)
  in
  let is_part_number (i, j, j_end) =
    adjecent_indexes (i, j, j_end)
    |> List.exists ~f:(fun coord -> valid_index coord && is_symbol coord)
  in
  let extract_int (i, j, j_end) =
    String.sub ~pos:j ~len:(j_end - j) engine.(i)
    |> int_of_string
  in
  engine,
  engine
  |> Array.concat_mapi ~f:extract_numbers
  |> Array.filter ~f:is_part_number
  |> Array.map ~f:(function (i, j, j_end) as coord -> i, j, j_end, extract_int coord)


let part1 input =
  let _engine, part_numbers = parse input in
  part_numbers
  |> Array.sum (module Int) ~f:(fun (_, _, _, part_number) -> part_number)
  |> sprintf "%d"

let part2 input =
  let engine, part_numbers = parse input in

  let extract_potential_gears i line : (int * int) array =
    line
    |> String.to_list
    |> List.concat_mapi ~f:(fun j c ->
      match c with
      | '*' -> [i, j]
      | _ -> []
    )
    |> Array.of_list
  in
  let adjecent_part_numbers (i', j') = 
    part_numbers
    |> Array.filter ~f:(fun (i, j, j_end, _) -> 
      List.mem ~equal:coord_eq (adjecent_indexes (i, j, j_end)) (i', j') 
    )
  in
  let gear_ratio = function
  | [|_, _, _, x; _, _, _, y|] -> x * y
  | _ -> 0
  in
  engine
  |> Array.concat_mapi ~f:extract_potential_gears
  |> Array.map ~f:adjecent_part_numbers
  |> Array.sum (module Int) ~f:gear_ratio
  |> sprintf "%d"
