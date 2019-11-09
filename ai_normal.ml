type t = {
  board: Board.t;
  name: string;
}

let alphalst = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

(** [rev_row_col loc] takes in a pair of ints in the range 0 to 9 and 
    converts it to a string representing a location on the board. *)
let rev_row_col loc : Command.location = 
  let (x, y) = loc in 
  List.nth alphalst x ^ string_of_int (y + 1)

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
  mutable tried_down : bool;
  mutable tried_left : bool;
  mutable tried_right : bool;
  mutable tried_up : bool;
}

(** [init_history ()] is a record of type history with no successful hits. *)
let init_history () = 
  {
    hit = "";
    tried_up = false; 
    tried_down = false; 
    tried_left = false; 
    tried_right = false; 
  }
let hit_history = init_history () 

(** [up_coor loc] gives the coordinate directly above [loc]. If the coordinate
    above doesn't exist, it gives the original [loc]. *)
let up_coor loc = 
  let (x, y) = Board.row_col loc in 
  try rev_row_col (x - 1, y)
  with exn -> rev_row_col (x, y)

(** [down_coor loc] gives the coordinate directly below [loc]. If the coordinate
    above doesn't exist, it gives the original [loc]. *)
let down_coor loc = 
  let (x, y) = Board.row_col loc in 
  try rev_row_col (x + 1, y)
  with exn -> rev_row_col (x, y)

(** [right_coor loc] gives the coordinate directly right of [loc]. If the 
    coordinate doesn't exist, it gives the original [loc]. *)
let right_coor loc = 
  let (x, y) = Board.row_col loc in 
  try rev_row_col (x, y + 1)
  with exn -> rev_row_col (x, y)

(** [left_coor loc] gives the coordinate directly left of [loc]. If the 
    coordinate doesn't exist, it gives the original [loc].*)
let left_coor loc = 
  let (x, y) = Board.row_col loc in 
  try rev_row_col (x, y - 1)
  with exn -> rev_row_col (x, y)

let reset_history hist = 
  hist.hit <- "";
  hist.tried_down <- false;
  hist.tried_left <- false;
  hist.tried_up <- false;
  hist.tried_right <- false

let rec shoot_random b = 
  try 
    let coor = random_coors () in 
    match Board.shoot (coor) b with
    | "It's a hit!" -> hit_history.hit <- coor; "It's a hit!"
    | msg -> msg
  with | _ -> shoot_random b

let rec shoot_left b = 
  let left = left_coor hit_history.hit in
  match Board.shoot (left) b with 
  | exception Board.InvalidLoc -> hit_history.tried_left <- true; shoot_right b
  | exception Board.DuplicateShot -> hit_history.tried_left <- true; shoot_random b
  | "It's a hit!" -> hit_history.hit <- left; "It's a hit!"
  | msg -> hit_history.tried_left <- true; msg
and shoot_right b = 
  let right = right_coor hit_history.hit in
  match Board.shoot (right) b with 
  | exception Board.InvalidLoc -> hit_history.tried_right <- true; shoot_left b
  | exception Board.DuplicateShot -> hit_history.tried_right <- true; shoot_random b
  | "It's a hit!" -> hit_history.hit <- right; "It's a hit!"
  | msg -> hit_history.tried_right <- true; msg

let rec shoot_up b = 
  let up = up_coor hit_history.hit in
  match Board.shoot (up) b with 
  | exception Board.InvalidLoc -> hit_history.tried_up <- true; shoot_down b
  | exception Board.DuplicateShot -> hit_history.tried_up <- true; shoot_random b
  | "It's a hit!" -> hit_history.hit <- up; "It's a hit!"
  | msg -> hit_history.tried_up <- true; msg
and shoot_down b = 
  let down = down_coor hit_history.hit in
  match Board.shoot (down) b with 
  | exception Board.InvalidLoc -> hit_history.tried_down <- true; shoot_up b
  | exception Board.DuplicateShot -> hit_history.tried_down <- true; shoot_random b
  | "It's a hit!" -> hit_history.hit <- down; "It's a hit!"
  | msg -> hit_history.tried_down <- true; msg

let rec shoot_ship c b = 
  if hit_history.hit = "" then 
    shoot_random b
  else begin
    if (hit_history.tried_left && hit_history.tried_down && 
        hit_history.tried_up && hit_history.tried_right) then begin
      reset_history hit_history;
      shoot_ship c b end
    else if hit_history.tried_up = false then
      shoot_up b
    else if hit_history.tried_down = false then 
      shoot_down b
    else if hit_history.tried_left = false then 
      shoot_left b
    else
      shoot_right b
  end 