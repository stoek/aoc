let lines = Advent.read_lines_from_file "day2.txt"

let print_string_tuples tuples =
  List.iter (fun (part1, part2) -> print_endline (part1 ^ ", " ^ part2)) tuples
;;

let print_string_ints = List.iter (fun i -> print_endline (string_of_int i))

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

print_string_tuples (even_tuples splitted)

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
(*let part1 = calc_score_for_tuples splitted
let () = List.map print_int part1
  *)

let part1 = List.fold_left ( + ) 0 (calc_score_for_tuples splitted);;

print_endline "Debug:";
print_string_ints (calc_score_for_tuples splitted);
print_endline "";
print_endline "";
print_endline "Part 1:";
print_endline (string_of_int part1)

type rps =
  | R
  | P
  | S
