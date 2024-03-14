open Core

let day = 10

let example_input_1 = {|
  -L|F7
  7S-7|
  L|7||
  -L-J|
  L|-JF
|}
let example_input_2 = {|
  7-F7-
  .FJ|7
  SJLL7
  |F--J
  LJ.LJ
|}

let flush () = Out_channel.flush Out_channel.stdout

let parse input =
  Stringlib.nonblank_lines input
  |> Array.of_list

let start_position map: int * int =
  map
  |> Array.find_mapi ~f:(fun i line -> String.index line 'S' |> Option.map ~f:(fun j -> i, j))
  |> Option.value_exn

let connections ~map (i, j) =
  match map.(i).[j] with
  | '.' -> []
  | 'S' -> [`north; `south; `east; `west ]
  | '|' -> [`north; `south]
  | '-' -> [`east; `west]
  | 'L' -> [`north; `east]
  | 'J' -> [`north; `west]
  | '7' -> [`south; `west]
  | 'F' -> [`south; `east]
  | _ -> failwith "invalid tile"

let dual_direction = function
  | `north -> `south
  | `south -> `north
  | `east -> `west
  | `west -> `east

let has_pipe ~map ~towards (i, j) =
  List.mem ~equal:Poly.(=) (connections ~map (i, j)) towards

let is_connected ~map (i, j) (i', j') =
  if i = i' && j = j' + 1 then
    has_pipe ~map ~towards:`east (i, j) && has_pipe ~map ~towards:`west (i', j')
  else if i = i' && j = j' - 1 then
    has_pipe ~map ~towards:`west (i, j) && has_pipe ~map ~towards:`east (i', j')
  else if i = i' + 1 && j = j' then
    has_pipe ~map ~towards:`north (i, j) && has_pipe ~map ~towards:`south (i', j')
  else if i = i' - 1 && j = j' then
    has_pipe ~map ~towards:`south (i, j) && has_pipe ~map ~towards:`north (i', j')
  else
    failwith "checking whether to non-adjectent are connected"

let is_inside ~map (i, j) =
  0 <= i && i < Array.length map &&
  0 <= j && j < String.length map.(0)

let adjecent_coordinates ~map (i ,j) =
  [ i + 1, j; i - 1, j; i, j - 1; i, j + 1]
  |> List.filter ~f:(is_inside ~map)

let connected_neighbors ~map (i, j) =
  adjecent_coordinates ~map (i, j)
  |> List.filter ~f:(is_connected ~map (i, j))

let next_tile ~map ~comming_from ~standing_on : (int * int) option =
  connected_neighbors ~map standing_on
  |> List.filter ~f:(fun tile -> Poly.(tile <> comming_from))
  |> List.hd

let rec any_two = function
  | [] -> []
  | x :: xs -> 
    List.map ~f:(fun y -> x, y) xs @ any_two xs

let find_cycle ~map ~start =
  let rec loop path comming_from standing_on =
    if Poly.(start = standing_on) then
      List.rev path
    else begin
      match next_tile ~map ~comming_from ~standing_on with
      | None -> failwith "no path"
      | Some next_tile -> 
          loop (standing_on :: path) standing_on next_tile
    end
  in
  connected_neighbors ~map start
  |> any_two
  |> List.find_map ~f:(fun (x, y) ->
    try Some (loop [] x y)
    with _ -> None
  )
  |> Option.value_exn

let coord_to_string (i, j) = sprintf "(%d, %d)" i j

let print_coord_list ?(msg="")xs = 
  xs |> List.map ~f:coord_to_string |> String.concat ~sep:" " |> printf "%s%s\n" msg

let part1 input =
  let map = parse input in
  let start = start_position map in
  printf "start position %s\n" (coord_to_string start); flush ();
  print_coord_list ~msg:"adjectent to start: " (adjecent_coordinates ~map start); flush ();
  printf "connected (1, 1) (2, 1)? %b\n" (is_connected ~map start (2, 1)); flush ();
  printf "has pipe 1: %b\n" (has_pipe ~map ~towards:`south (1, 1)); flush ();
  printf "has pipe 2: %b\n" (has_pipe ~map ~towards:`north (2, 1)); flush ();
  print_coord_list ~msg:"connected to start: " (connected_neighbors ~map start); flush ();
  let path = find_cycle ~map ~start in
  print_coord_list path;
  ""
let part2 _ = ""