let lines = Advent.read_lines_from_file "day3.txt"

let split_strings lst =
  let split_string str =
    let length = String.length str in
    let middle = length / 2 in
    let first_half = String.sub str 0 middle in
    let second_half = String.sub str middle (length - middle) in
    first_half, second_half
  in
  List.map split_string lst
;;

let group_strings lst =
  let rec group_helper acc group lst =
    match lst with
    | line1 :: line2 :: line3 :: rest ->
      let group' = line1 :: line2 :: line3 :: group in
      group_helper (group' :: acc) [] rest
    | _ -> List.rev acc
  in
  group_helper [] [] lst
;;

let find_common_characters_tuples tuples =
  let find_common_characters (index, tuple) =
    let first_string, second_string = tuple in
    let common_characters = ref [] in
    String.iter
      (fun c ->
        if (not (List.mem_assoc c !common_characters))
           && String.contains first_string c
           && String.contains second_string c
        then common_characters := (c, index) :: !common_characters)
      (first_string ^ second_string);
    (* Concatenate both strings for checking all characters *)
    !common_characters
  in
  List.mapi (fun index tuple -> find_common_characters (index, tuple)) tuples
;;

let find_common_characters_groups groups =
  let find_common_characters_group group =
    let common_characters =
      let merged_lines = String.concat "" group in
      let unique_characters =
        String.fold_left
          (fun acc c ->
            if (not (List.mem_assoc c acc))
               && List.for_all (fun line -> String.contains line c) group
            then (c, 0) :: acc
            else acc)
          []
          merged_lines
      in
      List.rev unique_characters
    in
    common_characters
  in
  List.map find_common_characters_group groups
;;

let assign_priority item =
  match Char.code item with
  | n when n >= 97 && n <= 122 -> n - 96
  | n when n >= 65 && n <= 90 -> n - 38
  | _ -> 0
;;

let assign_priority_tuple tuple =
  let item, _ = tuple in
  let priority = assign_priority item in
  priority
;;

let sum_list lst = List.fold_left ( + ) 0 lst
let tuples = split_strings lines
let common_tuples = find_common_characters_tuples tuples
let groups = group_strings lines
let common_groups = find_common_characters_groups groups

let priorities =
  List.map (fun sublist -> List.map assign_priority_tuple sublist) common_tuples
;;

let group_priorities =
  List.map (fun sublist -> List.map assign_priority_tuple sublist) common_groups
;;

let flattened_priorities = List.flatten priorities
let flattened_group_priorities = List.flatten group_priorities
let sum = sum_list flattened_priorities
let group_sum = sum_list flattened_group_priorities;;

Printf.printf "Sum (Split Strings): %d\n" sum;;
Printf.printf "Sum (Group Strings): %d\n" group_sum
