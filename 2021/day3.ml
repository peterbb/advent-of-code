open Core

let report = 
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:(Fn.compose Bytes.to_array Bytes.of_string)
    |> Array.of_list

let flip x =
    if Char.(x = '0') then '1' else '0'

let flip_all bits =
    bits
    |> Bytes.of_string
    |> Bytes.map ~f:flip
    |> Bytes.to_string

let most_common bits =
    let zeroes = Array.count ~f:(fun b -> Char.(b = '0')) bits in
    let ones = Array.count ~f:(fun b -> Char.(b = '1')) bits in
    if zeroes < ones then
        Some '1'
    else if ones < zeroes then
        Some '0'
    else 
        None

let most_common_exn bits =
    match most_common bits with
    | Some b -> b
    | None ->
        failwith "same number"


let int_of_binary b = int_of_string ("0b" ^ b)


module Part1 = struct
    let gamma =
        report
        |> Array.transpose_exn
        |> Array.map ~f:most_common_exn
        |> Array.to_list
        |> List.map ~f:(String.make 1)
        |> String.concat ~sep:""

    let epsilon = flip_all gamma

    let () = printf "%d\n" (int_of_binary gamma * int_of_binary epsilon)
end

module Part2 = struct

    let oxygen_generator_filter report i = 
        most_common report.(i) |> Option.value ~default:'1'

    let co2_scrubber_filter report i = 
        most_common report.(i) 
        |> Option.map ~f:(fun x -> if Char.(x = '0') then '1' else '0')
        |> Option.value ~default:'0'

    let find_number filter_char report =
        let rec loop report i =
            let c = filter_char (Array.transpose_exn report) i in
            let filter line = Char.(line.(i) = c) in
            let report = Array.filter ~f:filter report in
            if Array.length report <= 1 then
                report.(0)
            else
                loop report (i + 1)
        in 
        loop report 0
        |> Array.to_list
        |> String.of_char_list
        |> int_of_binary
        
    let oxygen_generator_rating = 
        find_number oxygen_generator_filter report

    let co2_scrubber_rating = 
        find_number co2_scrubber_filter report
        
    let () = printf "%d\n" (oxygen_generator_rating * co2_scrubber_rating)

end
