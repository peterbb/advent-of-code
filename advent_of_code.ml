open Core

let read_lines () = 
    In_channel.input_lines In_channel.stdin

let parse_input parser lexer = 
    Lexing.from_channel In_channel.stdin
    |> parser lexer
