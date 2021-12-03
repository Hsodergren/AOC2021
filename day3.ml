let to_uni = function
  | '0' -> `Zero
  | '1' -> `One
  | _ -> failwith "error"

let parse_line str =
  Seq.unfold (fun i -> try Some (String.get str i |> to_uni, succ i) with _ -> None) 0
  |> List.of_seq
  |> Array.of_list

let parse = Utils.map_lines parse_line
let input_file = "input/day3"

let count a l =
  let res = Array.make (Array.length (List.hd l)) 0 in
  List.iter (fun arr ->
      Array.iteri (fun i v ->
          if v = a then res.(i) <- res.(i) + 1
        ) arr
    ) l;
  res

let most_common length arr =
  let half = length /2 in
  Array.map (fun v -> if v > half then `One else `Zero) arr

let rev_iter f arr =
  Array.iteri (fun i _ ->
      f i arr.(Array.length arr - i -1)
    ) arr

let part1 =
  let input = parse input_file in
  let length = List.length input in
  let f a =
    let v = ref 0 in
    count a input |> most_common length |> rev_iter (fun i value -> match value with `One -> v := !v + 1 lsl i | _ -> ());
    !v
  in
  f `One * f `Zero
