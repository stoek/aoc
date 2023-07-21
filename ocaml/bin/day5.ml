let lines = Advent.read_lines_from_file "example5.txt"

let split_list lst =
  let rec split acc1 acc2 = function
    | [] -> List.rev acc1, List.rev acc2
    | "" :: tl -> List.rev acc1, List.rev_append tl acc2
    | hd :: tl -> split (hd :: acc1) acc2 tl
  in
  split [] [] lst
;;

let grid, instructions = split_list lines;;

print_endline "grid";;
List.iter print_endline grid;;
print_endline "instructions";;
List.iter print_endline instructions

let rec get_last_element = function
  | [ x ] -> x (* If there is only one element, it is the last element *)
  | _ :: tail -> get_last_element tail (* Recurse on the tail of the list *)
  | [] -> failwith "Empty list" (* Handle the case of an empty list *)
;;

type position = int
type value = int
type entry = value * position * int list

let track_positions input : entry list =
  let values = String.split_on_char ' ' input in
  let positions = ref [] in
  let current_position = ref 0 in
  List.iter
    (fun value ->
      if String.trim value <> ""
      then (
        let length = String.length value in
        positions := (int_of_string value, !current_position + 1, []) :: !positions;
        current_position := !current_position + length + 1))
    values;
  List.rev !positions
;;

let pos_in = get_last_element grid;;

print_endline pos_in

let positions = track_positions pos_in

(* Print the positions *)
let string_of_entry (value, position, lst) =
  "("
  ^ string_of_int value
  ^ ", "
  ^ string_of_int position
  ^ ", "
  ^ "["
  ^ String.concat "; " (List.map string_of_int lst)
  ^ "]"
  ^ ")"
;;

let () =
  List.iter
    (fun entry ->
      let entry_str = string_of_entry entry in
      print_endline entry_str)
    positions
;;
