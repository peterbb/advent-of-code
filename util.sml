infix 0 |>
fun x |> f = f x

infixr 3 $
fun f $ x = f x

fun print str =
    TextIO.output (TextIO.stdOut, str)

fun println str =
    print $ str ^ "\n"


structure Printf = struct
    fun id (x: 'a): 'a = x

    type 'a id = 'a -> 'a
    type 'a ignore = 'a -> unit
    type 'a force = (unit -> 'a) -> 'a

    val force: 'a force = fn p => p ()

    val % : 'a * ('b force -> 'c ignore -> 'd) -> 'd =
        fn (_, f) => f force ignore

    val fprintf: 'out -> ('out * 'a id -> 'b) -> 'b = fn out => fn f =>
        f (out, id)

    val printf: (TextIO.outstream * 'a id -> 'b) -> 'b =
         fn x => fprintf TextIO.stdOut x

    fun one ((out, f), make) g =
        let
            fun output s = TextIO.output (out, s)
            fun farg r p = make (fn s => r (fn () => (p (); output s)))
            fun e0 r = f (farg r)
        in
            g (out, e0)
        end

    fun ` x s = one (x, fn f => f s)

    fun spec to x = one (x, fn f => f o to)


    val B = fn z => spec Bool.toString z
    val I = fn z => spec Int.toString z
    val R = fn z => spec Real.toString z
    val C = fn z => spec Char.toString z
    val S = fn z => spec id z
end
