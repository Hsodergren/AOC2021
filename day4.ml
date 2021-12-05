let (let*) = Option.bind
module Board = struct
  type field = | Unmarked of int | Marked of int
  let value field = match field with | Unmarked v | Marked v -> v
  type t = field array array
  let size = 5

  let of_string str : t=
    String.split_on_char '\n' str
    |> List.map (fun line ->
           String.split_on_char ' ' line
           |> List.filter ((<>) "")
           |> List.map (fun str -> Unmarked (int_of_string str))
           |> Array.of_list
         )
    |> Array.of_list

  let get_row row t =
    t.(row)
  let get_col col t =
    Array.init size (fun row -> t.(row).(col))

  let all_marked = Array.for_all (function Marked _ -> true | Unmarked _ -> false) 
  let sum_unmarked t =
    let res = ref 0 in
    Array.iter (fun arr ->
        Array.iter (fun v ->
            match v with
            | Marked _ -> ()
            | Unmarked v -> res := !res + v
          ) arr
      ) t;
    !res

  exception Add of (int*int)
  let set_value v t =
    let add v t =
      let aux () = 
        Array.iteri (fun row arr ->
            Array.iteri (fun col a ->
                if v = value a then (t.(row).(col) <- Marked v; raise (Add (row,col)))
              ) arr
          ) t
      in
      try aux (); None with Add a -> Some a
    in
    let* (row,col) = add v t in
    let* solution = List.find_map (fun arr ->
                        if all_marked arr then Some arr else None
                      ) [get_row row t; get_col col t]
    in
    Some (Array.map value solution)

end

let test_string = {|7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
|}

let input_file = "input/day4"
let join_on_empty l =
  let rec aux acc res l =
    match l with
    | ""::tl -> aux [] (acc::res) tl
    | str::tl -> aux (str::acc) res tl
    | [] -> res
  in
  List.rev (aux [] [] l)
  |> List.map (String.concat "\n")

let parse_string str =
  match
    String.split_on_char '\n' str
    |> join_on_empty
  with
    hd::tl -> String.split_on_char ',' hd |> List.map int_of_string ,List.map Board.of_string tl
  | [] -> failwith "error"

let parse_file file = Utils.read_file file |> parse_string

let part1 =
  let numbers, boards = parse_file input_file in
  match
    List.find_map (fun v ->
        List.find_map (fun board ->
            let* _ = Board.set_value v board in
            Some (v,board)
          ) boards
      ) numbers
  with
  | Some (v,board) -> Some (v * Board.sum_unmarked board)
  | None -> None

let part2 =
  let numbers, boards = parse_file input_file in
  let rec aux numbers boards =
    match numbers, boards with
    | num::tl, [x] -> (
      match Board.set_value num x with
      | Some _-> Some (num * Board.sum_unmarked x)
      | None -> aux tl boards
    )
    | num::numtl, _ -> (
      match 
        List.filter_map (fun board ->
            match Board.set_value num board with
            | None -> Some board
            | Some _ -> None
          ) boards
      with
      | [] -> None
      | l -> aux numtl l
    )
    | [],_ -> None
  in
  aux numbers boards


