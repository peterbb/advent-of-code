open Core
open Printf

let day = 1

let example_input_1 = 
{|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|}

let example_input_2 =
{|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen|}

let digits_to_values = 
   ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]
   |> List.map ~f:(fun n -> n, n)

let spelled_out_digits_to_values =
   [ "one", "1"
   ; "two", "2"
   ; "three", "3"
   ; "four", "4"
   ; "five", "5"
   ; "six", "6"
   ; "seven", "7"
   ; "eight", "8"
   ; "nine", "9"
   ]

let k x y =
   match x, y with
   | None, None -> 0
   | Some _, None -> -1
   | None, Some _ -> 1
   | Some (i, _), Some (j, _) -> compare i j

let calibration_value extraction_table line = 
   let foo operation = 
      extraction_table
      |> List.map ~f:(fun (pattern, digit) ->
         operation line ~pattern
         |> Option.map ~f:(fun index -> index, digit)
      )
      |> List.min_elt ~compare:k
      |> Option.join
      |> Option.map ~f:snd
      |> Option.to_list
   in
   Stringlib.[ substr_first_index ; substr_last_index ]
   |> List.map ~f:foo
   |> List.concat
   |> String.concat
   |> Int.of_string

let sum_of_calibration_values extraction_table input = 
   input
   |> Stringlib.nonblank_lines
   |> List.sum ~f:(calibration_value extraction_table) (module Int)
   |> sprintf "%d"

let part1 = sum_of_calibration_values digits_to_values
let part2 = sum_of_calibration_values (digits_to_values @ spelled_out_digits_to_values)