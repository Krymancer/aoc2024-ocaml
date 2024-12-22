let get_input filename = In_channel.with_open_bin filename In_channel.input_all

let sanitize_input input =
    input
    |> String.split_on_char '\n'
    |> List.map String.trim

let make_pairs lines = List.map
      (fun line ->
         let parts =
           line
           |> String.trim
           |> String.split_on_char ' '
           |> List.filter (fun s -> s <> "")
           |> List.map int_of_string
         in
         match parts with
         | [x; y] -> (x, y)
         | _ ->
            failwith "Each line must contain exactly two integers."
      )
      lines

let part1 input =
  let lines = sanitize_input input in
  let pairs = make_pairs lines in

  let left_list, right_list = List.split pairs in

  let sorted_left = List.sort compare left_list in
  let sorted_right = List.sort compare right_list in

  let diffs = List.map2 (fun l r -> abs (l - r)) sorted_left sorted_right in

  let result =  List.fold_left ( + ) 0 diffs in
  string_of_int result

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

let part2 (input : string) : string =
  let lines = sanitize_input input in
  let pairs = make_pairs lines in

  let left_list, right_list = List.split pairs in

  let frequency_map =
    List.fold_left
      (fun acc num ->
         let current_count =
           match IntMap.find_opt num acc with
           | Some count -> count
           | None -> 0
         in
         IntMap.add num (current_count + 1) acc)
      IntMap.empty
      right_list
  in

  let sum_result =
    List.fold_left
      (fun acc num ->
         let freq =
           match IntMap.find_opt num frequency_map with
           | Some count -> count
           | None -> 0
         in
         acc + (num * freq))
      0
      left_list
  in

  string_of_int sum_result

let solve lines = 
  print_endline (part1 lines);
  print_endline (part2 lines)

let () = solve (get_input "problems/day01/input.txt")