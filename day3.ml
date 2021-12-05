let to_uni = function
  | '0' -> `Zero
  | '1' -> `One
  | _ -> failwith "error"

let parse_line str : [<`One | `Zero] array =
  Seq.unfold (fun i -> try Some (String.get str i |> to_uni, succ i) with _ -> None) 0
  |> List.of_seq
  |> Array.of_list

let parse = Utils.map_lines parse_line
let input_file = "input/day3"

let count_column f column l =
  List.fold_left (fun a arr ->
      if f arr.(column) then a+1 else a
    ) 0 l

let count a l =
  Array.init (Array.length (List.hd l)) (fun i -> count_column ((=) a) i l)

let most_common length arr =
  let half = length /2 in
  Array.map (fun v -> if v > half then `One else `Zero) arr

let rev_iter f arr =
  Array.iteri (fun i _ ->
      f i arr.(Array.length arr - i -1)
    ) arr

let calc_num arr =
    let v = ref 0 in
    rev_iter (fun i value -> match value with `One -> v := !v + 1 lsl i | _ -> ()) arr;
    !v
  
let part1 =
  let input = parse input_file in
  let length = List.length input in
  let f a =
    count a input |> most_common length |> calc_num
  in
  f `One * f `Zero

let part2 =
  let input = parse input_file in
  let rec aux1 column l f =
    let c = count_column ((=) `One) column l in
    let len = List.length l in
    let filter_f = f (len-c, c) column in
    let l = List.filter filter_f  l in
    if List.length l = 1 then List.hd l else aux1 (column + 1) l f
  in
  let ffun (o,z) c v =
    let v = v.(c) in
    if o = z then v = `One else if o > z then v = `Zero else v = `One 
  in
  calc_num (aux1 0 input ffun) * calc_num (aux1 0 input (fun a b c -> not (ffun a b c)))
