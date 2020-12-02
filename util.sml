infix 0 |>
fun x |> f = f x

infix 3 $
fun f $ x = f x


fun print str =
    TextIO.output (TextIO.stdOut, str)

fun println str =
    print $ str ^ "\n"
