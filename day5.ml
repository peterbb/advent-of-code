open Core

module List = struct
    include List

    let group_equals ~compare xs =
        let not_equal a b = compare a b <> 0 in
        List.sort ~compare xs
        |> List.group ~break:not_equal
end

let lines = 
    Lexing.from_channel In_channel.stdin
    |> Day5_parser.lines Day5_lexer.read 

let step n ~towards =
    if n < towards then
        n + 1
    else if n > towards then
        n - 1
    else
        failwith "step towards already there"

let expand_line (x1, y1, x2, y2) =
    let rec loop acc x1 y1 =
        if x1 = x2 && y1 = y2 then
            (x1, y1) :: acc
        else if x1 = x2 then
            loop ((x1, y1) :: acc) x1 (step y1 ~towards:y2)
        else if y1 = y2 then
            loop ((x1, y1) :: acc) (step x1 ~towards:x2) y1
        else 
            loop ((x1, y1) :: acc) (step x1 ~towards:x2) (step y1 ~towards:y2)
    in
    loop [] x1 y1

let expand_line_ignore_diag ((x1, y1, x2, y2) as line) =
    if x1 = x2 || y1 = y2 then
        expand_line line
    else
        []

let compare a b =
    let cmp1 (x0, _) (x1, _) = Int.compare x0 x1 in
    let cmp2 (_, y0) (_, y1) = Int.compare y0 y1 in
    Comparable.lexicographic [cmp1; cmp2] a b

let solve line_expander =
    lines
    |> List.concat_map ~f:line_expander
    |> List.group_equals ~compare
    |> List.count ~f:(fun l -> List.length l >= 2)
    |> printf "%d\n"

let () = solve expand_line_ignore_diag
let () = solve expand_line
