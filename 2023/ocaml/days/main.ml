open Core
open Stdio
open Core_bench

module type Day = sig
  val day: int
  val part1: string -> string
  val part2: string -> string
end

type testcase = 
  { part: [`p1 | `p2]
  ; input: string
  ; expect: [`is of string | `secret ]
  }

let run ?benchmark:(benchmark=false) day testcases =
  let module Day = (val day : Day) in
  let run_testcase i {part; input; expect} =
    Out_channel.flush Out_channel.stdout;
    let f = match part with
    | `p1 -> Day.part1
    | `p2 -> Day.part2
    in
    let name = sprintf "day %d part %d test %d" Day.day (match part with `p1 -> 1 | `p2 -> 2) i in
    let answer = f input in
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
      printf "🎅 %s failed. expected '%s', git '%s'.\n" name expect answer
    end;
    if benchmark then begin
      let bench = Bench.Test.create ~name (fun () -> ignore (f input)) in
      Command_unix.run (Bench.make_command [bench])
    end
  in
  testcases |> List.iteri ~f:run_testcase

let file filename = In_channel.read_all filename

let () = begin
  run (module Day01) 
  [ { part = `p1; input = Day01.example_input_1; expect = `is "142" }
  ; { part = `p1; input = file "inputs/01.txt"; expect = `secret }
  ; { part = `p2; input = Day01.example_input_2; expect = `is "281" }
  ; { part = `p2; input = file "inputs/01.txt"; expect = `secret }
  ];

  run (module Day02)
  [ { part = `p1; input = Day02.example_input_1; expect = `is "8" }
  ; { part = `p1; input = file "inputs/02.txt"; expect = `secret }
  ; { part = `p2; input = Day02.example_input_1; expect = `is "2286" }
  ; { part = `p2; input = file "inputs/02.txt"; expect = `secret }
  ];

  run (module Day03)
  [ { part = `p1; input = Day03.example_input_1; expect = `is "4361" }
  ; { part = `p1; input = file "inputs/03.txt"; expect = `secret }
  ; { part = `p2; input = Day03.example_input_1; expect = `is "467835"}
  ; { part = `p2; input = file "inputs/03.txt"; expect = `secret }
  ];

  run (module Day04)
  [ { part = `p1; input = Day04.example_input_1; expect = `is "13" }
  ; { part = `p1; input = file "inputs/04.txt"; expect = `secret }
  ; { part = `p2; input = Day04.example_input_1; expect = `is "30" }
  ; { part = `p2; input = file "inputs/04.txt"; expect = `secret }
  ];

  (*
  run (module day05)
  [ { part = `p1; input = day05.example_input_1; expect = `is "35" }
  ; { part = `p1; input = file "inputs/05.txt"; expect = `secret }
  ; { part = `p2; input = day05.example_input_1; expect = `is "46" }
  ; { part = `p2; input = file "inputs/05.txt"; expect = `secret }
  ];
     *)

  run (module Day06)
  [ { part = `p1; input = Day06.example_input_1; expect = `is "288" }
  ; { part = `p1; input = file "inputs/06.txt"; expect = `secret }
  ; { part = `p2; input = Day06.example_input_1; expect = `is "71503" }
  (* { part = `p2; input = file "inputs/06.txt"; expect = `secret } *)
  ];

  run (module Day07)
  [ { part = `p1; input = Day07.example_input_1; expect = `is "6440" }
  ; { part = `p1; input = file "inputs/07.txt"; expect = `secret }
  ; { part = `p2; input = Day07.example_input_1; expect = `is "5905" }
  ; { part = `p2; input = file "inputs/07.txt"; expect = `secret }
  ];

  ()
end