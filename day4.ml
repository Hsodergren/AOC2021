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
