let lines = Advent.read_lines_from_file "day2.txt"

let split_string_on_space str =
  let parts = String.split_on_char ' ' str in
  match parts with
  | [ part1; part2 ] -> part1, part2
  | _ -> failwith "Invalid input: expected exactly two parts separated by a space"
;;

let split_strings_on_space strings = List.map split_string_on_space strings
let splitted = split_strings_on_space lines

let even_tuples tuples =
  let transform_tuple tuple =
    match tuple with
    | "A", value -> "X", value
    | "B", value -> "Y", value
    | "C", value -> "Z", value
    | _ -> tuple
  in
  List.map transform_tuple tuples
;;

let calc_score_for_tuple tuple =
  let played =
    match tuple with
    | _, "X" -> 1
    | _, "Y" -> 2
    | _, "Z" -> 3
    | _ -> 0
  in
  let compared =
    match tuple with
    | "Z", "X" | "X", "Y" | "Y", "Z" -> 6
    | o, t when o = t -> 3
    | _ -> 0
  in
  compared + played
;;

let calc_score_for_tuples tpls = List.map calc_score_for_tuple (even_tuples tpls)
let part1 = List.fold_left ( + ) 0 (calc_score_for_tuples splitted);;

print_endline "";
print_endline "Part 1:";
print_endline (string_of_int part1)

let get_prebased_score a b =
  match a, b with
  | "X", 6 -> 2 + b
  | "Y", 6 -> 3 + b
  | "Z", 6 -> 1 + b
  | "X", 3 -> 1 + b
  | "Y", 3 -> 2 + b
  | "Z", 3 -> 3 + b
  | "X", 0 -> 3 + b
  | "Y", 0 -> 1 + b
  | "Z", 0 -> 2 + b
  | _, _ -> 0
;;

let calc_score_for_tuple_two tuple =
  let outcome =
    match tuple with
    | _, "X" -> 0
    | _, "Y" -> 3
    | _, "Z" -> 6
    | _ -> 0
  in
  get_prebased_score (fst tuple) outcome
;;

let calc_score_for_tuples_two tpls = List.map calc_score_for_tuple_two (even_tuples tpls)
let part2 = List.fold_left ( + ) 0 (calc_score_for_tuples_two splitted);;

print_endline "";
print_endline "Part 2:";
print_endline (string_of_int part2)
