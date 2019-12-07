open Helpers

exception OffBoard
exception Misaligned
exception WrongLength
exception OverlappingShips 
exception DuplicateShot
exception InvalidShipName

(** The type [ship_name] represents the name of each ship in the game. *)
type ship_name = string

(** The abstract type of values representing a ship's orientation. *)
type orn = Horz | Vert

(** The abstract type of values representing a ship. *)
type ship = {
  name : ship_name;
  size : int;
  mutable on_board: bool;
  mutable orientation: orn option;
  default : (Command.location * Command.location);
}

(** The abstract type of values representing a grid spot. *)
type spot =  Water | ShotWater | Ship of ship | HitShip of ship | Bomb | HitBomb

(** AF: the record
    [{board_size = 2; 
      grid = 
        [|
          [|Water; ShotWater|];
          [|Ship s1; HitShip s2|]
        |];
      ships = [s1; s2];
      player_name = "p1";
      status = Some "opponent missed";
      mode = "water"
    }]
    represents a board with size 2 where the player's name is "p1", the 
    player's opponent missed their last shot, position A1 is water, position A2
    is water that has been shot, position B1 is a cell of ship s1, and
    position B2 is a cell of ship s2 (and that shell has been shot). The game
    mode is "water".

    RI : Once board setup has ended, every ship [s] in [t.ships] 
    appears exactly [s.size] times in [t.grid], either as [Ship s] or
    [HitShip s]. The remaining cells are not a [Ship _] or a [Hitship _]. *)
type t = {
  board_size: int;
  grid: spot array array;
  ships: ship list;
  player_name: string;
  mutable status: string option;
  mode: string;
}

let default_board_size = 10

(** [random_coordinates board_size size] is a set of valid random locations
    on a board of size [board_size], used for randomly placing a ship of
    size [size]. *)
let random_coordinates board_size size : Command.location * Command.location = 
  (* horizontal ship placements with probability 0.5 *)
  if Random.bool() then 
    let letter = choose_random_letter board_size in
    let number = (Random.int board_size) + 1 in 
    if ((number + size) < board_size) then begin
      ((letter ^ string_of_int(number)), 
       (letter ^ string_of_int(number + size))) end
    else begin
      ((letter ^ string_of_int(number)), 
       (letter ^ string_of_int(number - size))) end
  else (* vertical ship placements with probability 0.5 *)
    let letter_num = (Random.int board_size) in 
    let number = string_of_int((Random.int board_size) + 1) in 
    if ((letter_num + size) < board_size) then begin
      ((get_letter letter_num) ^ number), 
      ((get_letter (letter_num + size)) ^ number) end
    else begin 
      (((get_letter letter_num) ^ number), 
       ((get_letter (letter_num - size)) ^ number)) end

(** [init_ship name size default] is an unplaced ship with the given [name],
    [size], and [default] position. *)
let init_ship name size default = {
  name=name;
  size=size;
  on_board=false;
  orientation=None;
  default=default;
} 

(** [init_ships ()] is a list of all ships in gameplay, with the appropriate
    names and sizes, [on_board] set to [false], no orientation, and [default]
    location values hardcoded. *)
let init_ships_default () = [
  init_ship "battleship" 4 ("a1", "a4");
  init_ship "cruiser" 2 ("b1", "b2");
  init_ship "carrier" 5 ("d2", "d6");
  init_ship "destroyer" 3 ("e2", "g2");
  init_ship "submarine" 3 ("f4", "f6");
]

let init_board_default player_name = {
  board_size = default_board_size;
  grid = Water |> Array.make_matrix default_board_size default_board_size;
  ships = init_ships_default ();
  player_name = player_name;
  status = None;
  mode = ""; (* default to water mode *)
}

let board_size b = b.board_size

(** [make_ships ships] is a [ship list] where each ship is initialized 
    according to its given name and size. *)
let make_ships ships =
  let count = ref 0 in
  let default size = (rev_row_col (!count, 0),
                      rev_row_col (!count, size-1)) in
  List.map (fun (name, size) ->
      count := (!count + 1);
      init_ship name size (default size)
    )
    ships

let init_board_from_file player_name filepath =
  let board_size, mode, ship_info =
    Custom_board_parser.get_board_from_file filepath in
  let ships = make_ships ship_info in
  {
    board_size=board_size;
    grid=Water |> Array.make_matrix board_size board_size;
    ships = ships;
    player_name = player_name;
    status=None;
    mode=mode;
  }

let min_ship_size b =
  List.fold_left (fun a sh -> if sh.size < a then sh.size else a) 100 b.ships

let player_name b = b.player_name

(** [long_string_of_ships shp_lst] is a long-form string description
    of [shp_lst]. *)
let long_string_of_ships shp_lst = 
  let to_string sh = 
    sh.name
    ^" (length "
    ^(sh.size |> string_of_int)
    ^")" in
  if List.length shp_lst = 0 then "None" else
    String.concat "; " (List.map to_string shp_lst) 

(** [get_ship str_name b] is the ship on [b] with string name [str_name]. *)
let get_ship str_name b =
  let sh_name = str_name in
  List.filter (fun s -> s.name=sh_name) b.ships |>
  function 
  | s::[] -> s
  | _ -> raise InvalidShipName

(** [on_board loc b] raises [OffBoard] iff [loc] refers to an invalid
    location on board [b]. *)
let on_board (loc : Command.location) (b : t) =
  let size = b.board_size in
  match row_col loc with 
  | (r, c) when (0 <= r && r < size) && (0 <= c && c < size) -> ()
  | _ -> raise OffBoard

(** [check_alignment_and_length loc1 loc2 s] raises [WrongLength] iff the
    inclusive distance between loc1 and loc2 does not equal the size of [s],
    or [Misaligned] iff [loc1] and [loc2] are not in the same row or
    column. *)
let check_alignment_and_length loc1 loc2 s =
  let (r1, c1), (r2, c2) = row_col loc1, row_col loc2 in
  if r1 = r2 then 
    if (c1 - c2 + 1 = s.size) || (c2 - c1 + 1 = s.size) then ()
    else raise WrongLength
  else if c1 = c2 then 
    if (r1 - r2 + 1 = s.size) || (r2 - r1 + 1 = s.size) then () 
    else raise WrongLength
  else raise Misaligned

(** [overlapping_ship_by_coors sh c1 c2 b] is true iff there are any ships
    (excepting [sh]) present in the span from [c1] to [c2] on [b]. *)
let overlapping_ship_by_coors sh (i_1, j_1) (i_2, j_2) b =
  for i = i_1 to i_2 do
    for j = j_1 to j_2 do 
      match b.(i).(j) with
      | Water -> ()
      | Ship sh' when sh' = sh -> ()
      | _ -> raise OverlappingShips 
    done
  done

(** [remove sh b] removes [sh] from [b] and replaces its grid cells with
    Water. If [sh] was not present on [b], it does nothing. *)
let remove sh b = 
  let remove_from_row r s = 
    Array.iteri (fun j spot -> if (spot = Ship s) then 
                    r.(j) <- Water else ()) r in
  sh.on_board <- false; sh.orientation <- None;
  Array.iter (fun r -> remove_from_row r sh) b.grid

(** [new_orientation l1 l2] is [Some Horz] if [l1] and [l2] are horizontal
    relative to one another and [Some Vert] if they are vertical.
    Precondition: [l1] and [l2] are in either the same row or column. *)
let new_orientation ((i1, j1):Helpers.coor_type) ((i2, j2):Helpers.coor_type)
  = if i1 = i2 then Some Horz else Some Vert

(** [place_single_ship sh l1 l2 b] places [sh] on [b],
    with its ends on the locations represented by [c1] and [c2].
    If [sh] was already on [b], it is removed before being re-placed.
    Raises:
    - OffBoard if [l1] or [l2] is off the game board
    - Misaligned if [l1] and [l2] are not in the same row or column
    - WrongLength if [l1] and [l2] are the wrong distance apart
    - OverlappingShips if the ship would overlap with a ship already
        present on [b]. *)
let place_single_ship sh l1 l2 b =
  (* let sh = get_ship s b in *)
  let ((i_1, j_1) as coors_1:Helpers.coor_type),
      ((i_2, j_2) as coors_2:Helpers.coor_type) = 
    ordered_strings l1 l2 in 
  on_board l1 b;
  on_board l2 b;
  check_alignment_and_length l1 l2 sh;
  overlapping_ship_by_coors sh coors_1 coors_2 b.grid;
  if sh.on_board then remove sh b else ();
  sh.on_board <- true;
  sh.orientation <- new_orientation coors_1 coors_2;
  for i = i_1 to i_2 do
    for j = j_1 to j_2 do 
      b.grid.(i).(j) <- Ship sh
    done
  done

let rec place s l1 l2 b =
  if s="default" then (
    List.fold_left
      (fun _ sh -> remove sh b)
      () b.ships;
    List.fold_left
      (fun _ sh -> let def1, def2 = sh.default in
        place_single_ship sh def1 def2 b)
      () b.ships 
  ) else if s="random" then try 
      List.fold_left (fun _ sh -> remove sh b)
        () b.ships;
      List.fold_left
        (fun _ sh ->
           let rand1, rand2 = random_coordinates b.board_size (sh.size - 1) in
           place_single_ship sh rand1 rand2 b)
        () b.ships 
    with exn -> place "random" "" "" b
  else
    place_single_ship (get_ship s b)
      l1 l2 b

let place_m_r s
    ((i_1, j_1) as l1:Helpers.coor_type) ((i_2, j_2) as l2:Helpers.coor_type)
    b =
  let sh = get_ship s b in 
  place_single_ship sh (rev_row_col l1) (rev_row_col l2) b

(** [is_dead s g] is true iff [s] is a sunken ship in the grid [g] (all
    of [s]'s cells have been hit). *)
let is_dead (s:ship) (g : spot array array) = 
  let dead_in_row (s:ship) (r : spot array) = 
    Array.fold_left
      (fun c sp -> c + (if sp = HitShip s then 1 else 0))
      0 r in
  s.size = (Array.fold_left (fun c r -> c + dead_in_row s r) 0 g)

let did_lose b = List.fold_left
    (fun true_so_far s -> true_so_far && is_dead s b.grid) true b.ships

(*BISECT-IGNORE-BEGIN*)
(** [is_on_board loc] is true if [(i, j)] is on the board. False otherwise. *)
let is_on_board (i, j) b = 
  (0 <= i && i < b.board_size) && (0 <= j && j < b.board_size)

(** [make_bomb_shot (i, j) b] updates board to have the (i, j) coordinate 
    counted as a shot. (Used for bomb damage.) *)
let rec make_bomb_shot (i, j) b =
  if is_on_board (i, j) b then 
    match b.grid.(i).(j) with 
    | Water -> b.grid.(i).(j) <- ShotWater; false
    | Ship s -> b.grid.(i).(j) <- HitShip s; true
    | Bomb -> b.grid.(i).(j) <- ShotWater; bomb_hit (i, j) b
    | _ -> false
  else false
(** [bomb_hit (i, j) b] updates the board to have damage from the bomb/mine. 
    (ie. All existing, non-hit spaces around the mine are now hit and any new 
    mines that get hit trigger another bomb explosion.) *)
and bomb_hit (i, j) b = 
  let hit_ship = ref false in
  let top_left = (i-1, j-1) in 
  let mid_left = (i, j-1) in 
  let bot_left = (i+1, j-1) in 
  let top_mid = (i-1, j) in 
  let center = (i, j) in  
  let bot_mid = (i+1, j) in 
  let top_right = (i-1, j+1) in 
  let mid_right = (i, j+1) in 
  let bot_right = (i+1, j+1) in 
  if make_bomb_shot top_left b then hit_ship := true;
  if make_bomb_shot mid_left b then hit_ship := true;
  if make_bomb_shot bot_left b then hit_ship := true;
  if make_bomb_shot top_mid b then hit_ship := true;
  if make_bomb_shot center b then hit_ship := true;
  if make_bomb_shot bot_mid b then hit_ship := true;
  if make_bomb_shot top_right b then hit_ship := true;
  if make_bomb_shot mid_right b then hit_ship := true;
  if make_bomb_shot bot_right b then hit_ship := true;
  !hit_ship

(** [mine_hit_your_ship boolean] is ["The mine damaged some of your ships."] 
    if [boolean] is true and is ["The mine didn't damage anything."] 
    otherwise. *)
let mine_hit_your_ship boolean =
  if boolean then "\nThe mine damaged some of your ships."
  else "\nThe mine didn't damage anything."

(** [mine_hit_op_ship boolean] is ["The mine damaged some of your opponent's 
    ships."] if [boolean] is true and is ["The mine didn't damage anything."] 
    otherwise. *)
let mine_hit_op_ship boolean =
  if boolean then " The mine damaged some of your opponent's ships."
  else "\nThe mine didn't damage anything."
(*BISECT-IGNORE-END*)

(** [shoot_helper coor b] is (s, m, n). [s] is a string message explaining
    the result of shooting location [coor] on [b]. [m] is [true] iff a
    ship has been shot. [n] is true iff a ship has been shot and is now
    "dead".
    The location on [b] represented by [coor] has now been shot; that
    location on [b] has been updated to reflect this information.
    Raises:
    - DuplicateShot if that location has already been shot.
    - InvalidLoc if that location is not on the board. *)
let shoot_helper (i, j) b =
  match b.grid.(i).(j) with 
  | exception Invalid_argument(_)  -> raise InvalidLoc
  | Water -> b.grid.(i).(j) <- ShotWater;
    b.status <- Some ("Your opponent shot "
                      ^(rev_row_col (i, j) |> String.uppercase_ascii)
                      ^" and missed.");
    "It's a miss!", false, false, false
  | Ship s -> b.grid.(i).(j) <- HitShip s;
    let sh_name = s.name in
    if is_dead s b.grid then (
      (b.status <- Some ("Your opponent sank your "^sh_name^"."));
      ("It's a hit! You sunk your opponent's " ^ sh_name ^ "!"), 
      true, true, false
    ) 
    else  (
      (b.status <- Some ("Your opponent shot your "^sh_name^"."));
      "It's a hit!", true, false, false
    )
  (*BISECT-IGNORE-BEGIN*)
  | Bomb -> let hit_ship = ref (bomb_hit (i, j) b) in
    (b.status <- Some ("\nYour opponent hit a mine at location " 
                       ^  (String.make 1 (Char.chr (i + 65))) 
                       ^  (string_of_int (j + 1)) ^ "." 
                       ^ (mine_hit_your_ship !hit_ship));
     ("You hit a mine at location " ^ (String.make 1 (Char.chr (i + 65))) ^ 
      (string_of_int (j + 1)) ^ "!" ^ (mine_hit_op_ship !hit_ship)),
     false, false, true)
  (*BISECT-IGNORE-END*)
  | _ -> raise DuplicateShot

let shoot l b =
  let message, success, _, bomb_success = shoot_helper (row_col l) b in
  message, success, bomb_success

let shoot_m_r (i, j) b =
  let _, success, killed, _ = shoot_helper (i, j) b in success, killed

let setup_status b = 
  let on_board = List.filter (fun s -> s.on_board) b.ships in
  let off_board = List.filter (fun s -> not s.on_board) b.ships in
  "On the board: "
  ^ (long_string_of_ships on_board)
  ^ "\nOff the board: "
  ^ (long_string_of_ships off_board)

let setup_status_m_r b =
  let off_board = List.filter (fun s -> not s.on_board) b.ships
                  |> List.map (fun s -> (s.name, s.size))
  in
  off_board

let status b = 
  let update = match b.status with 
    | None -> ""
    | Some m -> m in
  "You still have ships left. "^update

let complete b = 
  List.fold_left
    (fun true_so_far s -> true_so_far && s.on_board) true b.ships

let is_part_of_living_ship b (i, j) = 
  let g = b.grid in
  try
    match g.(i).(j) with
    | Ship s | HitShip s -> not (is_dead s g)
    | _ -> false
  with | _ -> false

(** [to_string_grid is_self b] is the grid representation of board [b].
    Represented as seen by the board's player if [is_self] is [true], and
    as seen by opponents otherwise. *)
let to_string_grid is_self b =
  let rec row_str self g = function
    | [] -> []
    | Water::t -> (if self then "w" else "?")::(row_str self g t)
    | ShotWater::t -> "x"::(row_str self g t)
    | (Ship s)::t -> (
        (
          if self then (if s.orientation=Some Vert then "|" else "-")
          else "?"
        )::(row_str self g t)
      )
    | (HitShip s)::t ->
      (if is_dead s g then "#" else (
          if s.orientation=Some Vert then "X|" else "X-"
        ))::(row_str self g t) 
    (*BISECT-IGNORE-BEGIN*)
    | Bomb::t -> (if self then "b" else "?")::(row_str self g t) 
    | HitBomb::t -> (if self then "B" else "?")::(row_str self g t) in
  (*BISECT-IGNORE-END*)
  b.grid |> Array.to_list
  |> List.map (fun r -> row_str is_self b.grid (Array.to_list r))

let string_self (b:t) = to_string_grid true b

let string_other b = to_string_grid false b

let is_unshot b (i, j) = try
    match (b.grid).(i).(j) with
    | Ship _ | Water -> true
    | _ -> false
  with | _ -> false

let rec place_mine b num = 
  if num > 0 then
    let i = (Random.int b.board_size) in 
    let j = (Random.int b.board_size) in 
    if b.grid.(i).(j) = Water then 
      (b.grid.(i).(j) <- Bomb; place_mine b (num - 1))
    else place_mine b num
  else ()

let graphics_mode b = b.mode


