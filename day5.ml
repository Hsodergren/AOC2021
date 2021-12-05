module Point = struct
  type t = int * int [@@deriving ord]
end
module Line = struct
  type t = | Vert of {x: int; y1: int; y2: int}
           | Hori of {y: int; x1: int; x2: int}
           | Diag of {p1: Point.t; p2: Point.t}

  let of_points (x1,y1) (x2,y2) =
    if x1 = x2 then
      Vert {x=x1; y1=min y1 y2; y2=max y1 y2}
    else if y1 = y2 then
      Hori {y=y1;x1=min x1 x2; x2=max x1 x2}
    else
      Diag {p1=(x1,y1); p2=(x2,y2)}

  let is_hor_vert = function | Vert _ | Hori _ -> true | Diag _ -> false

  let rec to_points line =
    match line with
    | Vert {x;y1;y2} -> (x,y1)::if y1 = y2 then [] else to_points (Vert {x;y1=succ y1;y2})
    | Hori {y;x1;x2} -> (x1,y)::if x1 = x2 then [] else to_points (Hori {y;x1=succ x1;x2})
    | Diag {p1=(x1,y1);p2=(x2,y2)} ->
       (x1,y1)::
         if (x1,y1) = (x2,y2) then []
         else
           to_points (Diag{p1=(if x1 > x2 then x1-1 else x1+1),(if y1 > y2 then y1-1 else y1+1);
                           p2=(x2,y2)})
end

module Board = struct
  module Pmap = Map.Make(Point)
  type t = int Pmap.t

  let add_line line t =
    let updatef = function
      | None -> Some 1
      | Some v -> Some (succ v)
    in
    List.fold_left (fun t point -> Pmap.update point updatef t) t (Line.to_points line)

  let empty = Pmap.empty
end

module Parser = struct
  open Angstrom

  let int = take_while (function | '0'..'9' -> true | _ -> false) >>| int_of_string

  let point =
    let* x = int <* char ',' in
    let+ y = int in
    x,y

  let line =
    let* p1 = point <* string " -> " in
    let+ p2 = point in
    Line.of_points p1 p2
end

let input_file = "input/day5"
let parse =
  Utils.map_lines (fun str -> Angstrom.(parse_string ~consume:Consume.All Parser.line str) |> Result.get_ok)

let run lines =
  List.fold_left (fun board line -> Board.add_line line board) Board.empty lines
  |> Board.Pmap.bindings
  |> List.filter (fun (_,i) -> i > 1)
  |> List.length

let part1 =
  run (parse input_file |> List.filter Line.is_hor_vert)

let part2 =
  run @@ parse input_file
