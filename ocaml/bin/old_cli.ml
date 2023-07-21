open Core
open Advent.day1

let tempt_print x = print_endline (string_of_int x)

let execute_day x =
  match x with
  | 1 -> tempt_print x
  | 2 -> tempt_print x
  | _ -> print_endline "Not yet implemented"
;;

let command =
  Command.basic
    ~summary:"Execute corresponding advent day"
    ~readme:(fun () -> "Nope sorry")
    Command.Param.(map (anon ("Day" %: int)) ~f:(fun day () -> execute_day day))
;;

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
