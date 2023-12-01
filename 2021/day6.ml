open Core
open Core_bench
open Advent_of_code

module IntMap = Map.Make(Int)

let raw_first_line =
    read_lines ()
    |> List.hd_exn

let rec compute_solution ~gen ~a0 ~a1 ~a2 ~a3 ~a4 ~a5 ~a6 ~a7 ~a8 =
    if gen = 0 then
        a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8
    else
        compute_solution
            ~gen:(gen - 1)
            ~a0:a1
            ~a1:a2
            ~a2:a3
            ~a3:a4
            ~a4:a5
            ~a5:a6
            ~a6:(a7 + a0)
            ~a7:a8
            ~a8:a0

let compute_init_school () = 
    raw_first_line
    |> String.split ~on:','
    |> List.map ~f:(fun n -> int_of_string n, 1)
    |> IntMap.of_alist_reduce ~f:(+)

let solve_only ~gen ~init_school =
    let g n = 
        IntMap.find init_school n 
        |> Option.value ~default:0 
    in
    compute_solution
        ~gen
        ~a0:(g 0)
        ~a1:(g 1)
        ~a2:(g 2)
        ~a3:(g 3)
        ~a4:(g 4)
        ~a5:(g 5)
        ~a6:(g 6)
        ~a7:(g 7)
        ~a8:(g 8)

let solve ~gen =
    solve_only ~gen ~init_school:(compute_init_school ())


let () = solve ~gen:80 |> printf "%d\n"
let () = solve ~gen:256 |> printf "%d\n"

let init_school = compute_init_school ()
let () = 
    Command.run (Bench.make_command [
        Bench.Test.create ~name:"gen 80, init state computed"
            (fun () -> ignore (solve ~gen:80));
        Bench.Test.create ~name:"gen 256, init state computed"
            (fun () -> ignore (solve ~gen:256));
        Bench.Test.create ~name:"gen 1000, init state computed"
            (fun () -> ignore (solve ~gen:1000));
        Bench.Test.create ~name:"gen 80, init state given"
            (fun () -> ignore (solve_only ~gen:80 ~init_school));
        Bench.Test.create ~name:"gen 256, init state given"
            (fun () -> ignore (solve_only ~gen:256 ~init_school));
        Bench.Test.create ~name:"gen 1000, init state given"
            (fun () -> ignore (solve_only ~gen:1000 ~init_school))
    ])
    
