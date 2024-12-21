let part1 input =
  let lines =
    input
    |> String.split_on_char '\n'
    |> List.map String.trim
  in

  let pairs =
    List.map
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
  in

  let left_list, right_list = List.split pairs in

  let sorted_left = List.sort compare left_list in
  let sorted_right = List.sort compare right_list in

  let diffs = List.map2 (fun l r -> abs (l - r)) sorted_left sorted_right in

  let result =  List.fold_left ( + ) 0 diffs in
  string_of_int result

let get_input filename = In_channel.with_open_bin filename In_channel.input_all

let solve lines = 
  print_endline (part1 lines)


let () = solve (get_input "problems/day01/input.txt")