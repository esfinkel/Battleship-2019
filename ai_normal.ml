open Helpers

type t = {
  board: Board.t;
}

let init () = {
  board = Board.init_board_default "💻";
}

let init_custom f = {
  board = Board.init_board_from_file "💻" f;
}


let get_board c = c.board

let place_all_ships c = Board.place "random" "" "" c.board

(** The abstract type of values representing the AI's shot history.
    (Gets updated based on successful hits and guesses around that hit.) *)
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

(*BISECT-IGNORE-BEGIN*)
(** [up_coor loc] gives the coordinate pair directly above [loc]. *)
let up_coor loc = 
  let (x, y) = Helpers.row_col loc in 
  (x - 1, y)

(** [down_coor loc] gives the coordinate pair directly below [loc]. *)
let down_coor loc = 
  let (x, y) = Helpers.row_col loc in 
  (x + 1, y)

(** [right_coor loc] gives the coordinate pair directly to the right of
    [loc].*)
let right_coor loc = 
  let (x, y) = Helpers.row_col loc in 
  (x, y + 1)

(** [left_coor loc] gives the coordinate pair directly to the left of
    [loc]. *)
let left_coor loc = 
  let (x, y) = Helpers.row_col loc in 
  (x, y - 1)

(** [reset_history hist] resets [hist] to have no successful hits and no
    attempted directions. *)
let reset_history hist = 
  hist.hit <- "";
  hist.tried_down <- false;
  hist.tried_left <- false;
  hist.tried_up <- false;
  hist.tried_right <- false
(*BISECT-IGNORE-END*)

(** [shoot_random b] shoots a random spot on board [b]. *)
let rec shoot_random b = 
  try 
    let coor = Helpers.random_coor_string (Board.board_size b) in 
    match Board.shoot (coor) b with
    | _, true, _ -> hit_history.hit <- coor; "It's a hit!"
    | _ -> ""
  with | _ -> shoot_random b

(*BISECT-IGNORE-BEGIN*)
(** [shoot_left b] shoots the spot to the left of the hit in [hit_history] and 
    updates [hit_history] accordingly.
    If the spot is off the board, it attempts to shoot right of the spot.
    If the spot is a duplicate shot or if it successfully sinks a ship, 
    it starts to shoot randomly again. *)
let rec shoot_left b = 
  let left = left_coor hit_history.hit in
  match Board.shoot_m_r (left) b with 
  | exception Helpers.InvalidLoc -> hit_history.tried_left <- true;
    shoot_right b
  | exception Board.DuplicateShot -> hit_history.tried_left <- true;
    shoot_random b
  | true, false -> hit_history.hit <- rev_row_col left; ""
  | false, false -> hit_history.tried_left <- true; ""
  | true, true -> reset_history hit_history; ""
  | _ -> failwith "impossible"

(** [shoot_right b] shoots the spot to the right of the hit in [hit_history]
    and updates [hit_history] accordingly.
    If the spot is off the board, it attempts to shoot left of the spot.
    If the spot is a duplicate shot or if it successfully sinks a ship, 
    it starts to shoot randomly again.  *)
and shoot_right b = 
  let right = right_coor hit_history.hit in
  match Board.shoot_m_r (right) b with 
  | exception Helpers.InvalidLoc -> hit_history.tried_right <- true;
    shoot_left b
  | exception Board.DuplicateShot -> hit_history.tried_right <- true;
    shoot_random b
  | true, false -> hit_history.hit <- rev_row_col right; ""
  | false, false -> hit_history.tried_right <- true; ""
  | true, true -> reset_history hit_history; ""
  | _ -> failwith "impossible"

(** [shoot_up b] shoots the spot above of the hit in [hit_history] and 
    updates [hit_history] accordingly.
    If the spot is off the board, it attempts to shoot below the spot.
    If the spot is a duplicate shot or if it successfully sinks a ship, 
    it starts to shoot randomly again.  *)
let rec shoot_up b = 
  let up = up_coor hit_history.hit in
  match Board.shoot_m_r (up) b with 
  | exception Helpers.InvalidLoc -> hit_history.tried_up <- true;
    shoot_down b
  | exception Board.DuplicateShot -> hit_history.tried_up <- true;
    shoot_random b
  | true, false -> hit_history.hit <- rev_row_col up; ""
  | false, false -> hit_history.tried_up <- true; ""
  | true, true -> reset_history hit_history; ""
  | _ -> failwith "impossible"

(** [shoot_down b] shoots the spot below of the hit in [hit_history] and 
    updates [hit_history] accordingly.
    If the spot is off the board, it attempts to shoot above of the spot.
    If the spot is a duplicate shot or if it successfully sinks a ship, 
    it starts to shoot randomly again. *)
and shoot_down b = 
  let down = down_coor hit_history.hit in
  match Board.shoot_m_r (down) b with 
  | exception Helpers.InvalidLoc -> hit_history.tried_down <- true;
    shoot_up b
  | exception Board.DuplicateShot -> hit_history.tried_down <- true;
    shoot_random b
  | true, false -> hit_history.hit <- rev_row_col down; ""
  | false, false -> hit_history.tried_down <- true; ""
  | true, true -> reset_history hit_history; ""
  | _ -> failwith "impossible"
(*BISECT-IGNORE-END*)

let rec shoot_ship c b = 
  if hit_history.hit = "" then 
    shoot_random b
    (*BISECT-IGNORE-BEGIN*)
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
      (*BISECT-IGNORE-END*)
  end 