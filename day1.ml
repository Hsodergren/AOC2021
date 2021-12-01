let input_file = "input/day1"
let parse_file = Utils.map_lines int_of_string

let part1 input =
  let f (prev,num) v =
    if v > prev then (v,num+1) else (v,num)
  in
  snd @@ List.fold_left f (List.hd input,0)  (List.tl input)

let rec part2 input =
  match input with
  | hd1::(hd2::hd3::_ as tl) -> hd1+hd2+hd3::part2 tl
  | _ -> []

let () =
  let input = parse_file input_file in
  Printf.printf "%d\n" @@ part1 input;
  Printf.printf "%d\n" @@ part1 (part2 input)
