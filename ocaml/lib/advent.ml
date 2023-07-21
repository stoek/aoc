let read_lines_from_file filename =
  let file = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line file in
      read_lines (line :: acc)
    with
    | End_of_file ->
      close_in file;
      List.rev acc
  in
  read_lines []
;;

let read_lines_to_in_channel file =
  In_channel.with_open_text file In_channel.input_all |> Str.(split (regexp "\n"))
;;
