
datatype square = OPEN | TREE

fun parse (#".") = OPEN
  | parse (#"#") = TREE
  | parse _ = raise (Fail "invalid quare char")

val map =
    read_lines ()
    |> List.map (List.map parse o String.explode)
    |> Array2.fromList

val map_width = Array2.nCols map
val map_height = Array2.nRows map

fun get (row, column) =
    if row < map_height then
        Some $ Array2.sub (map, row, column)
    else
        NONE

fun squares_encountered succ =
    let
        fun loop position acc =
            case get position of
            | SOME square => loop (succ position) (square :: acc)
            | NONE => List.rev acc
    in
        loop (0, 0) []
    end

fun trees_encountered succ =
    squares_encountered succ
    |> List.map (fn TREE => 1 | OPEN => 0)
    |> List.foldl op+ 0 

fun mksucc (row_inc, column_inc) =
    fn (row, column) =>
        (row + row_inc, (column + column_inc) mod map_width)

val slopes =
    [ (1, 1)
    , (3, 1)
    , (5, 1)
    , (7, 1)
    , (1, 2)
    ]

val () =
    slopes
    |> List.map Pair.swap
    |> List.map mksucc
    |> List.map trees_encountered
    |> List.map IntInf.fromInt
    |> List.foldl IntInf.* (IntInf.fromInt 1)
    |> IntInf.toString
    |> println


