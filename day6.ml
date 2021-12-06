let input_file = "input/day6"
let parse_file file =
  String.split_on_char ',' (Utils.read_file file)
  |> List.map int_of_string

let test = [3;4;3;1;2]

let rec duplicate_p1 n fishes =
  print_endline (string_of_int n);
  let rec aux fishes acc =
    match fishes with
    | [] -> acc
    | 0::tl -> aux tl (6::8::acc)
    | hd::tl -> aux tl (hd-1::acc)
  in
  if n = 0
  then fishes
  else duplicate (n-1) (aux fishes [])

module FishMap = Map.Make(Int)
let init fishes =
  FishMap.
  let updatef

let rec duplicate_p2 n fishes =
  print_endline (string_of_int n);
  let rec aux fishes acc =
    match fishes with
    | [] -> acc
    | 0::tl -> aux tl (6::8::acc)
    | hd::tl -> aux tl (hd-1::acc)
  in
  if n = 0
  then fishes
  else duplicate (n-1) (aux fishes [])

let part1 = duplicate 80 test
