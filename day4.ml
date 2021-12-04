open Core
open Advent_of_code

module String = struct
    include String

    let not_empty s =
        Int.(length s > 0)
end

let lines =
    read_lines ()

let draws = 
    List.hd_exn lines
    |> String.split ~on:','
    |> List.map ~f:int_of_string

let make_row line =
    line
    |> String.split ~on:' '
    |> List.filter ~f:String.not_empty
    |> List.map ~f:int_of_string
    |> List.map ~f:(fun n -> (false, n))
    
let boards =
    List.tl_exn lines
    |> List.map ~f:String.strip
    |> List.filter ~f:String.not_empty
    |> List.map ~f:make_row
    |> List.chunks_of ~length:5

let mark_on_board draw board =
    let mark_spot spot =
        match spot with
        | _, num when num = draw -> true, num
        | _ -> spot
    in
    List.map ~f:(List.map ~f:mark_spot) board

let board_wins board =
    let winning_row = List.for_all ~f:fst in
    let has_row_win = List.exists ~f:winning_row in
    has_row_win board || has_row_win @@ List.transpose_exn board

let declare_winner ~last_draw board =
    let sum_not_selected =
        List.concat board
        |> List.filter ~f:(Fn.compose not fst)
        |> List.map ~f:snd
        |> List.sum (module Int) ~f:ident
    in
    printf "sum: %d\n" sum_not_selected;
    printf "last_draw: %d\n" last_draw;
    printf "answer: %d\n" (sum_not_selected * last_draw)

let solve_board draws board =
    let rec loop i draws board =
        match draws with
        | [] -> None
        | draw::draws ->
            let board = mark_on_board draw board in
            if board_wins board then
                Some (i, draw, board)
            else
                loop (i + 1) draws board
    in
    loop 0 draws board

let solve_problem ~find =
    let compare (i, _, _) (j, _, _) = Int.compare i j in
    let find_elem = match find with
        | `First -> List.min_elt ~compare
        | `Last -> List.max_elt ~compare
    in
    boards 
    |> List.map ~f:(solve_board draws)
    |> List.concat_map ~f:Option.to_list
    |> find_elem
    |> Option.iter ~f:(fun (_, last_draw, board) -> 
        declare_winner ~last_draw board
    )

let () = solve_problem ~find:`First
let () = solve_problem ~find:`Last
