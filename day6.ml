let input_file = "input/day6"
let parse_file file =
  String.split_on_char ',' (Utils.read_file file)
  |> List.map int_of_string

let rec simulate n init stepf =
  if n = 0
  then init
  else simulate (n-1) (stepf init) stepf

let rec stepf_p1 fishes =
  let rec aux fishes acc =
    match fishes with
    | [] -> acc
    | 0::tl -> aux tl (6::8::acc)
    | hd::tl -> aux tl (hd-1::acc)
  in
  aux fishes []

module FishMap = Map.Make(Int)
let init fishes =
  List.fold_left (fun acc f ->
      FishMap.update f (function | Some v -> Some (v+1) | None -> Some 1) acc
    ) FishMap.empty fishes

let stepf_p2 fishes =
  FishMap.fold (fun key value acc ->
      let add_value = function | Some v -> Some (v+value) | None -> Some value in
      if key = 0
      then FishMap.update 6 add_value (FishMap.update 8 add_value acc)
      else FishMap.update (key-1) add_value acc
    ) fishes FishMap.empty

let test = parse_file input_file

let part1 = simulate 80 test stepf_p1 |> List.length
let part2 = simulate 256 (init test) stepf_p2 |> FishMap.bindings |> List.fold_left (fun acc (_,v) -> acc+v) 0
