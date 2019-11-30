exception ParsingError
exception InvalidBoardFile of string

open Yojson.Basic.Util

(** [check_board b] is [b] iff no exception is thrown; if not in "space"
    mode, [b] is now in "water" mode.
    Raises [InvalidBoardFile s] (where [s] is an explanatory message) if:
    - Board size not in 1..15
    - Duplicate ship name
    - There are not 1..10 ships
    - There is a ship with length not in 1.."the board size"
    - There are not 1..20 total ship cells
    - There are too many ship cells for the board size
    - The json has "ship_names" and "ship_sizes" lists of different
          lengths.*)
let check_board (b_size, mode, ships) same_num_ship_names_sizes =
  let rec count a = function
    | [] -> 0
    | v::t -> (if v=a then 1 else 0) + (count a t) in
  let num_cells = List.fold_left (fun acc (_, sz) -> acc + sz) 0 ships in
  let assert_raise equality issue =
    try assert equality with | _ -> raise (InvalidBoardFile issue) in
  let ship_names = (List.map (fun (n, _) -> n) ships) in
  assert_raise same_num_ship_names_sizes ("make sure you have same number "
                                          ^"of ship_names and ship_sizes!");
  assert_raise (0 < b_size && b_size <= 15)
    "board_size should be in 1..15.";
  assert_raise (List.fold_left
                  (fun b (nm, _) -> b && 1 = count nm ship_names)
                  true ships)
    "Ships should not have duplicate names.";
  assert_raise (0 < List.length ships && List.length ships <= 10)
    "There should be 1 to 10 ships.";
  List.fold_left (fun () (_, sz) -> assert_raise (0 < sz && sz <= b_size)
                     "Each ship should have length 1..board_size." ) () ships; 
  assert_raise (0 < num_cells && num_cells <= 20)
    "There must be 1..20 total ship cells.";
  assert_raise (num_cells*5 < b_size*b_size)
    "Too many ship cells for the board size.";
  let mode = match String.lowercase_ascii mode with
    | "space" -> "space"
    | _ -> "" in (* default to water mode *)
  (b_size, mode, ships)


let get_board_from_file f =
  let make_ships j =
    let names = j |> member "ship_names" |> to_list
                |> List.map to_string |> List.map String.lowercase_ascii in
    let sizes = j |> member "ship_sizes" |> to_list |> List.map to_int in
    try List.map2 (fun a b -> (a, b)) names sizes, true with
    | _ -> [], false
  in
  let j = try Yojson.Basic.from_file f with | _ -> raise ParsingError in
  let ships, valid_len = make_ships j in
  let board_tup = 
    (
      j |> member "board_size" |> to_int,
      j |> member "mode" |> to_string,
      ships
    ) in
  check_board board_tup valid_len

