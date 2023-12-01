open Core
open Printf

module List = struct
    include List

    let split ~on list = 
        list
        |> List.group ~break:(fun x _ -> String.(x = on))
        |> List.map ~f:(List.filter ~f:(fun x -> String.(x <> on)))

    let rec of_fn n ~f =
        match n with
        | 0 -> []
        | n when n > 0 -> f (n - 1) :: of_fn (n - 1) ~f
        | _ -> failwith "List.of_fn: negative number"

    let rec repeat elem ~times = of_fn times ~f:(fun _ -> elem)
end

module String = struct
    include String
    let split_at str idx =
        sub str ~pos:0 ~len:idx, sub str ~pos:idx ~len:(length str - idx)

    let split_middle str =
        split_at str (length str / 2)
end

module Fn = struct
    include Fn
    let uncurry f (x, y) = f x y
end

module Set = struct
    include Set

    let inter_list (type a) (type cmp) cmp sets =
        List.sum 
            (module struct
                type nonrec t = (a, cmp) t
                let zero = union_list cmp sets
                let (+) = inter
            end)
            sets
            ~f:Fn.id

end


let peek ~f x = f x; x

module Day1 = struct
    let exec n lines =
        lines
        |> List.split ~on:"" 
        |> List.map ~f:(List.sum (module Int) ~f:int_of_string)
        |> List.sort ~compare:(Comparable.reverse Int.compare)
        |> Fn.flip List.take n
        |> List.sum ~f:Fn.id (module Int)

    let part1 = exec 1 
    let part2 = exec 3
end


module Day2 = struct
    let to_action = function
        | "A" | "X" -> `Rock
        | "B" | "Y" -> `Paper
        | "C" | "Z" -> `Scissors
        | _ -> failwith "unknown input"

    let to_outcome = function
        | "X" -> `Lose
        | "Y" -> `Draw
        | "Z" -> `Win
        | _ -> failwith "unknown input"

    let winner_against = function
        | `Rock -> `Paper
        | `Paper -> `Scissors
        | `Scissors -> `Rock

    let exec ~parse_me ~strategy lines =
        let score_outcome (opponent_shape, my_shape) = 
            if Poly.(opponent_shape = my_shape) then 3
            else if Poly.(winner_against opponent_shape = my_shape) then 6
            else 0
        in
        let score_shape = function
        | _, `Rock -> 1
        | _, `Paper -> 2
        | _, `Scissors -> 3
        in
        let score game = score_outcome game + score_shape game in
        lines
        |> List.map ~f:(String.lsplit2_exn ~on:' ')
        |> List.map ~f:(fun (x, y) -> to_action x, parse_me y)
        |> List.map ~f:(fun (x, y) -> x, strategy (x, y))
        |> List.sum (module Int) ~f:score

    let part1 =
        exec ~parse_me:to_action ~strategy:(fun (_, my_move) -> my_move)

    let lose_against = Fn.compose winner_against winner_against

    let ensure_outcome (opponent_shape, desired_outcome) = 
        let reaction = match desired_outcome with
            | `Draw -> Fn.id
            | `Win -> winner_against
            | `Lose -> lose_against
        in
        reaction opponent_shape

    let part2 =
        exec ~parse_me:to_outcome ~strategy:ensure_outcome
end

module Day3 = struct
    let parse_rucksack line = 
        let left, right = String.split_middle line in
        let compartment x = 
            String.to_array x |> Set.of_array (module Char)
        in
        compartment left, compartment right

    let score c =
        let open Char in
        if between c ~low:'a' ~high:'z' then
            1 + (to_int c - to_int 'a')
        else if between c ~low:'A' ~high:'Z' then
            27 + (to_int c - to_int 'A')
        else 
            failwith "unknown rucksack item"

    let exec ~compartments ~group_size lines =
        lines
        |> List.map ~f:parse_rucksack
        |> List.map ~f:(Fn.uncurry compartments)
        |> List.chunks_of ~length:group_size
        |> List.map ~f:(Set.inter_list (module Char))
        |> List.map ~f:(Set.sum (module Int) ~f:score)
        |> List.sum (module Int) ~f:Fn.id


    let part2 = exec ~compartments:Set.union ~group_size:3

    let part1 = exec ~compartments:Set.inter ~group_size:1
end

module Range(T: Comparable.S): sig
    type t
    val range: from:T.t -> to_:T.t -> t
    val is_subset: t -> of_:t -> bool
    val inter: t -> t -> t
    val is_inhabited: t -> bool
end = struct

    type t = T.t * T.t
    let range ~from ~to_ = from, to_

    let is_subset (x_from, x_to) ~of_:(of_from, of_to) =
        T.(of_from <= x_from && x_to <= of_to)

    let inter (x_from, x_to) (y_from, y_to) =
        (T.max x_from y_from), (T.min x_to y_to)

    let is_inhabited (x_from, x_to) =
        T.(x_from <= x_to)

end

module Day4 = struct
    module IntRange = Range(Int)

    let parse line =
        let nums = line
            |> String.tr ~target:'-' ~replacement:' '
            |> String.tr ~target:',' ~replacement:' '
            |> String.split ~on:' '
            |> List.map ~f:int_of_string
        in
        match nums with
        | [n; m; o; p] -> IntRange.range ~from:n ~to_:m, IntRange.range ~from:o ~to_:p
        | _ -> failwith "parse error"

    let exec ~condition lines =
        lines
        |> List.map ~f:parse
        |> List.map ~f:(fun (x, y) -> condition x y || condition y x)
        |> List.sum (module Int) ~f:Bool.to_int


    let part1 = exec ~condition:(fun elf1 elf2 -> 
        IntRange.is_subset elf1 ~of_:elf2
    )

    let part2 = exec ~condition:(fun elf1 elf2 -> 
        IntRange.inter elf1 elf2 |> IntRange.is_inhabited
    )
end

module Day5 = struct
    module P = String.Search_pattern

    let move_pattern = P.create "move "
    let from_pattern = P.create "from "
    let to_pattern = P.create "to "
    let double_space_pattern = P.create "  "

    let remove pattern in_ =
        P.replace_all pattern ~with_:"" ~in_

    let parse_figure lines =
        let desc, lines = match List.rev lines with
            | desc::lines -> desc, lines
            | _ -> failwith "parse error"
        in
        let width = 
            desc
            |> remove double_space_pattern
            |> String.strip
            |> String.split ~on:' '
            |> List.map ~f:int_of_string
            |> List.last_exn
        in
        let height = List.length lines in
        Array.init width ~f:(fun col -> 
            List.of_fn height ~f:(fun row -> 
                List.nth_exn lines (height - 1 - row)
                |> Fn.flip String.get ((col * 4) + 1) 
            )
            |> List.filter ~f:(Char.(<>) ' ')
            |> List.rev
        )

    let parse_move line =
        line
        |> remove move_pattern
        |> remove from_pattern
        |> remove to_pattern
        |> String.split ~on:' '
        |> List.map ~f:int_of_string
        |> function
            | [amount; source; target] -> (amount, source - 1, target - 1)
            | _ -> failwith "parse error"

    let parse lines = 
        match List.split lines ~on:"" with
        | [figure; moves] -> parse_figure figure, List.map ~f:parse_move moves
        | _ -> failwith "parse error"

    let expand_move (amount, source, target) =
        List.repeat (1, source, target) ~times:amount

    let update state (amount, source, target) = 
        let (pickup, remaning) = List.split_n state.(source) amount in
        state.(source) <- remaning;
        state.(target) <- pickup @ state.(target)

    let exec ~transform_move lines =
        let figure, moves = parse lines in
        List.concat_map ~f:transform_move moves
        |> List.iter ~f:(update figure);
        figure
        |> Array.map ~f:List.hd_exn
        |> Array.to_list
        |> String.of_char_list

    let part1 = exec ~transform_move:expand_move
    let part2 = exec ~transform_move:(fun x -> [x])
end


let day (type a) day ~part exec expected_list (module S : Stringable.S with type t = a) =
    List.mapi expected_list ~f:(fun i expected_result ->
    let input = i + 1 in
    let filename = sprintf "day-%02d-%d.txt" day input in
    let lines = In_channel.read_lines filename in
    let actual_result = exec lines in
    let prefix = sprintf "day %02d part %d input %d" day part input in
    let actual = S.to_string actual_result in
    let expected = S.to_string expected_result in
    if Poly.(=) actual expected then
        Result.Ok (sprintf "%s: %s" prefix actual)
    else
        Result.Error (sprintf "%s: expected %s, got %s" prefix expected actual)
        )

let main () =
    printf "\n";
    let days = [
        day 1 ~part:1 Day1.part1 [24000; 70509] (module Int);
        day 1 ~part:2 Day1.part2 [45000; 208567] (module Int);
        day 2 ~part:1 Day2.part1 [15; 8392] (module Int);
        day 2 ~part:2 Day2.part2 [12; 10116] (module Int);
        day 3 ~part:1 Day3.part1 [157; 8176] (module Int);
        day 3 ~part:2 Day3.part2 [70; 2689] (module Int);
        day 4 ~part:1 Day4.part1 [2; 518] (module Int);
        day 4 ~part:2 Day4.part2 [4; 909] (module Int);
        day 5 ~part:1 Day5.part1 ["CMZ"; "TLFGBZHCN"] (module String);
        day 5 ~part:2 Day5.part2 ["MCD"; "QRQFHFWCL"] (module String);
    ] in
    let ok, failed= List.concat days |> List.partition_result in
    List.iter ok ~f:(printf "ok %s\n");
    List.iter failed ~f:(printf "ERROR: %s\n");
    begin if not (List.is_empty failed) then
        printf "%d ERRORS!!!\n" (List.length failed)
    end


let () = main ()

