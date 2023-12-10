open Core

let day = 9

let example_input_1 = {|
  0 3 6 9 12 15
  1 3 6 10 15 21
  10 13 16 21 30 45
|}

let digits =
  let open Re in
  seq
  [ opt (char '-')
  ; rep1 digit
  ]
  |> compile

let parse (input: string): int list list =
  let parse_line line =
    line 
    |> Re.matches digits
    |> List.map ~f:int_of_string
  in
  input
  |> Stringlib.nonblank_lines
  |> List.map ~f:parse_line

let adjecent_elements xs =
  let rec loop acc = function
    | [] | [_] -> List.rev acc
    | (x :: ((y :: _) as xs)) -> loop ((x, y) :: acc) xs
  in
  loop [] xs

let difference_at_each_step xs =
  adjecent_elements xs
  |> List.map ~f:(fun (x, y) -> y - x)

let all_zeroes xs = List.for_all ~f:((=) 0) xs

let print_list offset xs =
  xs
  |> List.map ~f:(sprintf "%2d")
  |> String.concat ~sep:"  "
  |> printf "%s%s\n" (String.make offset ' ')

let rec difference_tree xs: int list list =
  if all_zeroes xs then
    [ xs ]
  else
    xs :: (difference_at_each_step xs |> difference_tree)

let rec print_difference_tree ?offset:(offset=0) = function
  | [] -> ()
  | xs :: xss ->
    print_list offset xs;
    print_difference_tree ~offset:(offset + 2) xss

let rec extrapolate (xss: int list list): int list list = 
  match xss with
  | [] -> failwith "empty tree"
  | [ xs ] -> [ xs @ [ 0 ] ]
  | xs :: xss -> 
    let xss = extrapolate xss in
    let previous_value = List.last_exn xs in
    let diff = List.hd_exn xss |> List.last_exn in
    (xs @ [ previous_value + diff ]) :: xss

let rec extrapolate_backwards (xss: int list list): int list list = 
  match xss with
  | [] -> failwith "empty tree"
  | [ xs ] -> [ [ 0 ] @ xs ]
  | xs :: xss -> 
    let xss = extrapolate_backwards xss in
    let next_value = List.hd_exn xs in
    let diff = List.hd_exn xss |> List.hd_exn in
    ([ next_value - diff ] @ xs) :: xss

let part1 input = 
  let input = parse input in
  let difference_trees = input |> List.map ~f:difference_tree in
  let solutions = difference_trees |> List.map ~f:extrapolate in
  (* List.iter2_exn difference_trees solutions ~f:(fun problem solution ->
    printf "Input:\n";
    print_difference_tree problem;
    printf "Output:\n";
    print_difference_tree solution;
    printf "\n\n";
  ); *)
  solutions
  |> List.sum (module Int) ~f:(fun xss -> xss |> List.hd_exn |> List.last_exn)
  |> sprintf "%d"

let part2 input =
  let input = parse input in
  let difference_trees = input |> List.map ~f:difference_tree in
  let solutions = difference_trees |> List.map ~f:extrapolate_backwards in
  solutions
  |> List.sum (module Int) ~f:(fun xss -> xss |> List.hd_exn |> List.hd_exn)
  |> sprintf "%d"