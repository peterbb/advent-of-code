
datatype square = OPEN | TREE

fun parse (#".") = OPEN
  | parse (#"#") = TREE
  | parse _ = raise (Fail "invalid quare char")

val map =
    read_lines ()
    |> List.map (List.map parse o String.explode)
    |> Array2.fromList

fun succ (i, j) =
    (i + 1, j + 3)

fun get (i, j) =
    if i < Array2.nRows map then
        Some $ Array2.sub (map, i, j mod Array2.nCols map)
    else
        NONE

fun squares_encountered () =
    let
        fun loop position acc =
            case get position of
            | SOME square => loop (succ position) (square :: acc)
            | NONE => List.rev acc
    in
        loop (0, 0) []
    end

val () =
    squares_encountered ()
    |> List.map (fn TREE => 1 | OPEN => 0)
    |> List.foldl op+ 0 
    |> Int.toString
    |> println


