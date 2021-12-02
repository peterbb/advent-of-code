open Core
open Day2_command

let commands = 
    Lexing.from_channel In_channel.stdin
    |> Day2_parser.commands Day2_lexer.read 

module type MEANING = sig
    type t
    val init : t
    val forward: int -> t -> t
    val down: int -> t -> t
    val up: int -> t -> t
    val display: t -> unit
end

module Exec (M: MEANING) = struct
    let apply pos command =
        match command with
        | Forward n -> M.forward n pos
        | Down n -> M.down n pos
        | Up n -> M.up n pos

    let final = commands
        |> List.fold ~init:M.init ~f:apply
        |> M.display
end

module Part1 = struct
    type t = {
        depth: int;
        hpos: int;
    }

    let forward n pos = 
        { pos with hpos = pos.hpos + n }

    let down n pos =
        { pos with depth = pos.depth + n }

    let up n pos = 
        { pos with depth = pos.depth - n }
        

    let init = { depth = 0; hpos = 0; }

    let display {depth; hpos} =
        let product = depth * hpos in
        printf "depth: %d hpos: %d = %d\n" depth hpos product

end

module _ = Exec(Part1)


module Part2 = struct
    type t = {
        depth: int;
        hpos: int;
        aim: int;
    }

    let down n state = { state with aim = state.aim + n }

    let up n state = { state with aim = state.aim - n }

    let forward  n state = 
        { state with
            hpos = state.hpos + n;
            depth = state.depth + state.aim * n;
        }

    let init = { depth = 0; hpos = 0; aim = 0 }

    let display = function {depth; hpos; aim} ->
        let product = depth * hpos in
        printf "depth: %d hpos: %d aim: %d. answer: %d\n"
            depth hpos aim product
end

module _ = Exec(Part2)
