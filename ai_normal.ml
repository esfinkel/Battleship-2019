type t = {
  board: Board.t;
  name: string;
}

(** [row_col loc] is the [(row, column)] coordinate pair corresponding
    to [loc]. *)
let row_col (loc : Command.location) : (int*int) =
  let r = Str.regexp "\\([a-z]\\)\\([0-9]+\\)" in
  let pull_regex s =
    Str.global_replace r "\\1 \\2" (String.lowercase_ascii s)
    |> String.split_on_char ' ' in
  let index (c:char) : int = Char.code c - 97 in
  let tup = function
    | letter::number::[] -> (
        String.get letter 0 |> index,
        Stdlib.int_of_string number - 1
      )
    | _ -> raise (Failure "error")
  in
  loc |> pull_regex |> tup

let alphalst = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

let rev_row_col loc : Command.location = 
  let (x, y) = loc in 
  List.nth alphalst x ^ string_of_int y

let seed_random () = Unix.time () |> int_of_float |> Random.init

let init () =
  let () = seed_random () in
  {
    board = Board.init_board "";
    name = ""
  }

let get_board c = c.board

let place_all_ships c = Board.place "random" "" "" c.board

let random_coors () =
  let yaxis = Char.chr ((Random.int 10) + 65) |> String.make 1 in
  let xaxis = string_of_int ((Random.int 10) + 1) in
  yaxis ^ xaxis

(* To help AI keep track of guesses around a hit. *)
type history = {
  mutable hit : Command.location; 
  mutable tried_up : bool;
  mutable tried_down : bool;
  mutable tried_right : bool;
  mutable tried_left : bool
}

let init_history () = 
  {
    hit = "";
    tried_up = false; 
    tried_down = false; 
    tried_right = false; 
    tried_left = false; 
  }
let hit_history = init_history () 

(** [up_coor loc] gives the coordinate directly above [loc]. If the coordinate
    above doesn't exist, it gives the coordinate directly below [loc]. *)
let rec up_coor loc = 
  let (x, y) = row_col loc in 
  try rev_row_col (x - 1, y)
  with exn -> down_coor loc 
(** [down_coor loc] gives the coordinate directly below [loc]. If the coordinate
    above doesn't exist, it gives the coordinate directly above [loc]. *)
and down_coor loc = 
  let (x, y) = row_col loc in 
  try rev_row_col (x + 1, y)
  with exn -> up_coor loc 

(** [right_coor loc] gives the coordinate directly right of [loc]. If the 
    coordinate doesn't exist, it gives the coordinate directly left of [loc]. *)
let rec right_coor loc = 
  let (x, y) = row_col loc in 
  try rev_row_col (x, y + 1)
  with exn -> left_coor loc 
(** [left_coor loc] gives the coordinate directly left of [loc]. If the 
    coordinate doesn't exist, it gives the coordinate directly right of [loc].*)
and left_coor loc = 
  let (x, y) = row_col loc in 
  try rev_row_col (x, y - 1)
  with exn -> right_coor loc 

let shoot_left b = 
  failwith ""

let shoot_right b = 
  failwith ""

let shoot_down b = 
  failwith ""

let shoot_up b = 
  failwith ""

let rec shoot_ship b = try Board.shoot (random_coors ()) b with
  | _ -> shoot_ship b