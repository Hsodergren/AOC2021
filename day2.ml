module Pos = struct
  type t = (int * int)

  let up i (x,y) = x,y-i
  let down i (x,y) = x,y+i
  let forward i (x,y) = x+i ,y
  let origo = (0,0)
end

module PosAim = struct
  type t = { pos: Pos.t
           ; aim: int }

  let start = {pos = Pos.origo; aim = 0}
  let aim i t = {t with aim = t.aim+i}
  let forward i t = {t with pos=Pos.forward i t.pos |> Pos.down (t.aim*i)}
  let origo = {pos=(0,0);aim=0}
end

module Inst = struct
  let of_string_1 str =
    let l = String.split_on_char ' ' str in
    match l with
    | ["forward"; int] -> Pos.forward @@ int_of_string int
    | ["up"; int] -> Pos.up @@ int_of_string int
    | ["down"; int] -> Pos.down @@ int_of_string int
    | _ -> failwith "error"

  let of_string_2 str =
    let l = String.split_on_char ' ' str in
    match l with
    | ["forward"; int] -> PosAim.forward @@ int_of_string int
    | ["up"; int] -> PosAim.aim @@ int_of_string int * -1
    | ["down"; int] -> PosAim.aim @@ int_of_string int
    | _ -> failwith "error"
end

let rec sequence start ts =
  match ts with
  | hd::tl -> sequence (hd start) tl
  | [] -> start

let input_file = "input/day2"

let part1 =
  Utils.map_lines Inst.of_string_1 input_file
  |> sequence Pos.origo

let part2 =
  Utils.map_lines Inst.of_string_2 input_file
  |> sequence PosAim.origo
