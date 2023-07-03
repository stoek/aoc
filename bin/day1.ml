let lines = Advent.read_lines_from_file "day1.txt"

let rec group input result =
  match input with
  | [] -> result
  | "" :: rest -> group rest (0 :: result)
  | cals :: rest ->
    group
      rest
      (match result with
       | [] -> [ int_of_string cals ]
       | hd :: tail -> (hd + int_of_string cals) :: tail)
;;

let rec max_by_stan list cur =
  match list with
  | [] -> cur
  | hd :: tl -> if hd > cur then max_by_stan tl hd else max_by_stan tl cur
;;

print_endline "Part 1:";
print_int (max_by_stan (group lines []) 0);
print_endline ""

let rec top3 list (x, y, z) =
  match list with
  | [] -> x, y, z
  | hd :: tl ->
    top3
      tl
      (match x, y, z with
       | x, y, _ when hd > x -> hd, x, y
       | x, y, _ when hd > y -> x, hd, y
       | x, y, z when hd > z -> x, y, hd
       | _ -> x, y, z)
;;

let s1, s2, s3 = top3 (group lines []) (0, 0, 0);;

print_endline "Part 2:";
print_int (s1 + s2 + s3);
print_endline ""
