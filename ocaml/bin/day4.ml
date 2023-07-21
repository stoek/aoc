let lines = Advent.read_lines_from_file "day4.txt"

let convert_string str =
  let split_range range =
    match String.split_on_char '-' range with
    | [ start; stop ] -> int_of_string start, int_of_string stop
    | _ -> failwith "Invalid range format"
  in
  let pairs = String.split_on_char ',' str in
  match pairs with
  | [ pair1; pair2 ] ->
    let tuple1 = split_range pair1 in
    let tuple2 = split_range pair2 in
    tuple1, tuple2
  | _ -> failwith "Invalid input format"
;;

let check_fully_contained tpl =
  let is_fully_contained (start1, stop1) (start2, stop2) =
    (start1 <= start2 && stop1 >= stop2) || (start2 <= start1 && stop2 >= stop1)
  in
  match is_fully_contained (fst tpl) (snd tpl) with
  | true -> 1
  | false -> 0
;;

let check_somewhat_contained tpl =
  let range_overlap (range1_start, range1_end) (range2_start, range2_end) =
    range1_start <= range2_end && range2_start <= range1_end
  in
  match range_overlap (fst tpl) (snd tpl) with
  | true -> 1
  | false -> 0
;;

let converted_lines = List.map convert_string lines
let overlapping_pairs = List.map check_fully_contained converted_lines
let res = List.fold_left ( + ) 0 overlapping_pairs;;

print_int res

let overlapping_pairs2 = List.map check_somewhat_contained converted_lines
let res2 = List.fold_left ( + ) 0 overlapping_pairs2;;

print_endline "";;
print_int res2;;
print_endline ""
