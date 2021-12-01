let map_lines f file =
  let inc = open_in file in
  let rec loop () =
    match input_line inc with
    | str -> f str::loop ()
    | exception End_of_file -> close_in inc; []
  in
  loop ()
