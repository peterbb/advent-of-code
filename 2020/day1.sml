infix 0 |>
fun x |> f = f x

fun input () =
    case TextIO.inputLine TextIO.stdIn of
    | NONE => []
    | SOME x => (Int.fromString x |> Option.valOf)  :: input ()

fun doIt elements = 
    let
        exception FOUND of int

        val len = Vector.length elements

        fun rangeLoop from_index body =
            let 
                fun loop i =
                    if i < len then
                        (body (i, Vector.sub (elements, i)); loop (i + 1))
                    else
                        ()
            in
                loop from_index
            end
    in
        rangeLoop 0 (fn (i, a) =>
            rangeLoop (i + 1) (fn (j, b) =>
                rangeLoop (j + 1) (fn (k, c) =>
                    if a + b + c = 2020 then
                        raise (FOUND (a * b * c))
                    else
                        ())))
        handle FOUND n =>
            TextIO.output (TextIO.stdOut, Int.toString n ^ "\n")
    end
    

val () = 
    input ()
    |> List.filter (fn x => x <= 2020)
    |> Vector.fromList
    |> doIt
