open Core
open Stdio
open Core_bench

module type Day = sig
  val day: int
  type input
  module Parser : sig
    type token
    val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> input
  end
  val token: Sedlexing.lexbuf -> Parser.token
  val part1: input -> string
  val part2: input -> string
end

type testcase = 
  { part: [`p1 | `p2]
  ; input: string
  ; expect: [`is of string | `secret ]
  }

let run ?benchmark:(benchmark=false) day testcases =
  let module Day = (val day : Day) in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Day.Parser.parse in
  let parse input = 
    let lexbuf = Sedlexing.Utf8.from_string input in
    let lexer = Sedlexing.with_tokenizer Day.token lexbuf in
    parser lexer
  in
  let run_testcase i {part; input; expect} =
    Out_channel.flush Out_channel.stdout;
    let f = match part with
    | `p1 -> Day.part1
    | `p2 -> Day.part2
    in
    let name = sprintf "day %d part %d test %d" Day.day (match part with `p1 -> 1 | `p2 -> 2) (i + 1) in
    let answer = f (parse input) in
    let expect = match expect with
      | `is x -> x
      | `secret -> 
        Map.find Solutions.solutions Day.day
        |> Option.value ~default:("?", "?")
        |> match part with `p1 -> fst | `p2 -> snd
    in
    begin if String.(answer = expect) then
      printf "🎄 %s\n" name
    else 
      printf "🎅 %s failed. expected '%s', got '%s'.\n" name expect answer
    end;
    if benchmark then begin
      let input = parse input in
      let bench = Bench.Test.create ~name (fun () -> ignore (f input)) in
      Command_unix.run (Bench.make_command [bench])
    end
  in
  testcases |> List.iteri ~f:run_testcase

let file filename = In_channel.read_all filename

let () = begin
  run (module Day01) 
  [ { part = `p1; input = Day01.example_input; expect = `is "11" }
  ; { part = `p1; input = file "inputs/01.txt"; expect = `secret }
  ; { part = `p2; input = Day01.example_input; expect = `is "31" }
  ; { part = `p2; input = file "inputs/01.txt"; expect = `secret }
  ];

  ()
end