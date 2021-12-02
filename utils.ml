let map_lines f file =
  let inc = open_in file in
  let rec loop () =
    match input_line inc with
    | str -> f str::loop ()
    | exception End_of_file -> close_in inc; []
  in
  loop ()

let map_lines_str f str =
  String.split_on_char '\n' str |> List.map f
