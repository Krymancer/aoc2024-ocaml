let get_input filename = In_channel.with_open_bin filename In_channel.input_all

let sanitize_input input =
    input
    |> String.split_on_char '\n'
    |> List.map String.trim
let get_list_from_line line =
    line
     |> String.split_on_char ' '
     |> List.map String.trim
     |> List.map int_of_string

let rec difference = function
    | x1 :: ( x2 :: _ as tail) -> (x1 - x2) :: difference tail 
    | _ -> []

let all_positives list = List.for_all (fun x -> x > 0) list
let all_negative list = List.for_all (fun x -> x < 0) list

let all_in_range list =
  List.for_all (fun x -> let d = abs x in d >= 1 && d <= 3) list

let is_stricly_monotonic list = 
  let differences = difference list in
  (all_negative differences || all_positives differences) && all_in_range differences

let int_of_bool b = if b then 1 else 0

let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t

let part1 input =
  let safes = input
  |> sanitize_input
  |> List.map get_list_from_line
  |> List.map is_stricly_monotonic
  |> List.map int_of_bool in
  string_of_int (sum safes)
  

let part2 (_ : string) : string =
  failwith "TODO"

let solve lines = 
  print_endline (part1 lines);
  print_endline (part2 lines)

let () = solve (get_input "problems/day02/input.txt")