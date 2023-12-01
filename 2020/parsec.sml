

structure ParserCore :> sig
    type 'a t

    val exec: 'a t -> 'a option

    val return: 'a -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val seq: 'a t -> 'b t -> 'b t
    val map: ('a -> 'b) -> 'a t -> 'b t
    val option: 'a t -> 'a option t
    val eof: unit t
    val char_where: (char -> bool) -> char t

end = struct 
    type stream = TextIO.StreamIO.instream

    type 'a t = stream -> ('a * stream) option

    fun exec parser =
       TextIO.getInstream TextIO.stdIn
       |> parser
       |> Option.map (fn (a, _) => a)
        

    fun return x stream =
        SOME (x, stream)

    fun bind parser_a parser_b stream =
        parser_a stream
        |> Option.map (fn (a, stream) => parser_b a stream)
        |> Option.join

    fun seq parser_a parser_b stream =
        parser_a stream
        |> Option.map (fn (_, stream) => parser_b stream)
        |> Option.join

    fun map f parser stream =
        case parser stream of
        | NONE => NONE
        | SOME (x, stream) => SOME (f x, stream)

    fun option parser stream =
        case parser stream of
        | NONE => SOME (NONE, stream)
        | SOME (x, stream) => SOME (SOME x, stream)

    fun eof stream =
        if TextIO.StreamIO.endOfStream stream
        then SOME ((), stream)
        else NONE

    fun char_where predicate stream =
        case TextIO.StreamIO.input1 stream of
        | NONE => NONE
        | SOME (c, stream) =>
            if predicate c
            then SOME (c, stream)
            else NONE
end

structure ParserExt : sig
    type 'a t = 'a ParserCore.t

    val list: 'a t -> 'a list t
    val commute_list: 'a t list -> 'a list t
    val pair: 'a t -> 'b t -> ('a * 'b) t
    val char: char t
    val exact_char: char -> unit t
    val exact_string: string -> unit t
    val rest_of_line: string t
    val number: int t
end = struct
    open ParserCore

    fun list parser =
        bind (option parser) (fn result => case result of
        | NONE => return []
        | SOME x => map (fn xs => x::xs) (list parser)
        )

    fun pair a_parser b_parser =
        bind a_parser (fn a =>
        bind b_parser (fn b =>
        return (a, b)
        ))

    fun exact_char c =
        bind (char_where (fn c' => c = c')) (fn _ => return ())

    val char =
        char_where (fn _ => true)

    fun commute_list [] =
            return []
      | commute_list (p::ps) =
            bind p (fn x =>
            bind (commute_list ps) (fn xs =>
            return (x :: xs)))

    fun exact_string string =
        String.explode string
        |> List.map exact_char
        |> commute_list
        |> map (fn _ => ())
        
    val rest_of_line =
        let
            val finish = return o String.implode o List.rev

            fun loop acc =
                bind (option char) (fn c =>
                case c of
                | SOME c => 
                    if c = #"\n"
                    then finish acc
                    else loop (c :: acc)
                | NONE => finish acc)
        in
            loop []
        end

    fun nonempty_list parser =
        bind parser (fn x =>
        bind (list parser) (fn xs =>
        return (x::xs)))

    val number =
        nonempty_list (char_where Char.isDigit)
        |> map String.implode
        |> map $ (Option.valOf o Int.fromString)
        

end

structure Parser = struct
    open ParserCore
    open ParserExt

    infix <*>
    fun f <*> g = pair f g 

    infix <|>
    fun f <|> g = map g f

    infix //
    fun f // g = bind f (fn a => seq g (return a))
end
