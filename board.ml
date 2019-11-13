open Helpers

(* For generating actual random numbers. *)
let () = Random.self_init ();
exception OffBoard
exception Misaligned
exception WrongLength
exception OverlappingShips 
exception NoShip
exception DuplicateShot
exception InvalidLoc
exception InvalidShipName

(** The type [ship_name] represents the name of each ship in the game. *)
type ship_name = Battleship | Cruiser | Carrier | Destroyer | Submarine

(** The abstract type of values representing a ship. *)
type orn = Horz | Vert
type ship = {
  name : ship_name;
  size : int;
  mutable on_board: bool;
  mutable orientation: orn option;
  default : (Command.location * Command.location)
}

(** The abstract type of values representing a grid spot. *)
type spot =  Water | ShotWater | Ship of ship | HitShip of ship

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
    | _ -> raise InvalidLoc
  in
  loc |> pull_regex |> tup

(** [ordered l1 l2] is [(l1_x, l1_y), (l2_x, l2_y)], the coordinates of
    location [l1] and location [l2] respectively, except they are swapped
    if given in the reverse order.
    Precondition to comparability is that [l1] and [l2] are in either the
    same row or the same column. *)
let ordered l1 l2 = let pos1, pos2 = (row_col l1), (row_col l2) in
  if pos1 < pos2 then pos1, pos2 else pos2, pos1

(** AF: the record
    [{ grid = [
        [|
          [|Water; ShotWater|];
          [|Ship s1; HitShip s2|]
        |];
      ships = [s1; s2];
      player_name = "p1";
      status = Some "opponent missed"
    }]
    represents a board where the player's name is "p1", the player's
    opponent missed their last shot, position A1 is water, position A2
    is water that has been shot, position B1 is a cell of ship s1, and
    position B2 is a cell of ship s2 (and that shell has been shot).
    RI : Once board setup has ended, every ship [s] in [t.ships] 
    appears exactly [s.size] times in [t.grid], either as [Ship s] or
    [HitShip s]). The remaining cells are [Water] or [ShotWater]. 
*)
type t = {
  grid: spot array array;
  ships: ship list;
  player_name: string;
  mutable status: string option;
}

let board_size = 10

(* Used for getting random letters. (Didn't have to hardcode this but figured
   it was easier since our gameboard is always the same size.) *)
let lst = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

(** [choose_random_letter ()] is a random letter represented as a string from 
    the first ten letters of the alphabet in the list [lst] defined above. *)
let choose_random_letter () =  
  List.nth lst (Random.int 10)

(** [random_coordinates size] is a set of valid random locations used for 
    randomly placing a ship of size [size]. *)
let random_coordinates size : Command.location * Command.location = 
  (* horizontal ship placements with probability 0.5 *)
  if Random.bool() then 
    let letter = choose_random_letter () in
    let number = (Random.int 10) + 1 in 
    if ((number + size) < board_size) then begin
      ((letter ^ string_of_int(number)), 
       (letter ^ string_of_int(number + size))) end
    else begin
      ((letter ^ string_of_int(number)), 
       (letter ^ string_of_int(number - size))) end
  else (* vertical ship placements with probability 0.5 *)
    let letter_num = (Random.int 10) in 
    let number = string_of_int((Random.int 10) + 1) in 
    if ((letter_num + size) < board_size) then begin
      ((List.nth lst letter_num) ^ number), 
      ((List.nth lst (letter_num + size)) ^ number) end
    else begin 
      (((List.nth lst letter_num) ^ number), 
       ((List.nth lst (letter_num - size)) ^ number)) end

(** [init_ship name size default] is an unplaced ship with the given [name],
    [size], and [default]. *)
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
let init_ships () = [
  init_ship Battleship 4 ("a1", "a4");
  init_ship Cruiser 2 ("b1", "b2");
  init_ship Carrier 5 ("d2", "d6");
  init_ship Destroyer 3 ("e2", "g2");
  init_ship Submarine 3 ("f4", "f6");
]


let init_board player_name = {
  grid=Water |> Array.make_matrix board_size board_size;
  ships = init_ships ();
  player_name= player_name;
  status=None;
}

let player_name b = b.player_name

(** [ship_of_string str] is the ship_name with string name [str]. *)
let ship_of_string = function
  | "battleship" -> Battleship
  | "cruiser" -> Cruiser
  | "carrier" -> Carrier
  | "destroyer" -> Destroyer
  | "submarine" -> Submarine
  | _ -> raise InvalidShipName

(** [string_of_ship shp] is the string name of ship_name [shp]. *)
let string_of_ship = function
  | Battleship -> "battleship"
  | Cruiser -> "cruiser"
  | Carrier -> "carrier"
  | Destroyer -> "destroyer"
  | Submarine -> "submarine"

(** [long_string_of_ships shp_lst] is a long-form string description
    of [shp_lst]. *)
let long_string_of_ships shp_lst = 
  let to_string sh = 
    (string_of_ship sh.name)
    ^" (length "
    ^(sh.size |> string_of_int)
    ^")" in
  if List.length shp_lst = 0 then "None" else
    String.concat "; " (List.map to_string shp_lst) 

(** [get_ship str_name b] is the ship on [b] with string name [str_name]. *)
let get_ship str_name b =
  let sh_name = ship_of_string str_name in
  List.filter (fun s -> s.name=sh_name) b.ships |>
  function 
  | s::[] -> s
  | _ -> raise NoShip

(** [on_board loc b] raises [OffBoard] iff [loc] refers to an invalid
    location  on board [b]. *)
let on_board (loc : Command.location) (b : t) =
  let size = board_size in
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

(** [overlapping_ship_by_coors c1 c2 b] is true iff there are any ships
    present in the span from [c1] to [c2] on [b]. *)
let overlapping_ship_by_coors sh (x_1, y_1) (x_2, y_2) b =
  for x = x_1 to x_2 do
    for y = y_1 to y_2 do 
      match b.(x).(y) with
      | Water -> ()
      | Ship sh' when sh' = sh -> ()
      | _ -> raise OverlappingShips 
    done
  done


(** [overlapping_ship l1 l2 b] is true iff there are any ships present in
    the span from [l1] to [l2] on [b]. *)
let overlapping_ship sh l1 l2 b =
  let c1, c2 = ordered l1 l2 in overlapping_ship_by_coors sh c1 c2 b


(** [remove n b] is [()]. If a ship with name [n] was present in [b], it
    has been removed, and the cells are replaced with Water.
    Raises:
    - NoShip if that ship has not been placed. *)
let remove sh b = 
  let remove_from_row r s = 
    Array.iteri (fun j spot -> if (spot = Ship s) then 
                    r.(j) <- Water else ()) r in
  if sh.on_board 
  then (sh.on_board <- false; sh.orientation <- None;
        Array.iter (fun r -> remove_from_row r sh) b.grid;)
  else raise NoShip

(* I don't think this is right, but it works? *)
let new_orientation (x1, y1) (x2, y2) =
  if x1 <> x2 then Some Vert else Some Horz

(* todo document *)
let place_single_ship s l1 l2 b =
  let sh = get_ship s b in
  let (((x_1, y_1) as coors_1), ((x_2, y_2) as coors_2)) = ordered l1 l2 in 
  on_board l1 b;
  on_board l2 b;
  check_alignment_and_length l1 l2 sh;
  overlapping_ship sh l1 l2 b.grid;
  if sh.on_board then remove sh b else ();
  sh.on_board <- true;
  sh.orientation <- new_orientation coors_1 coors_2;
  for x = x_1 to x_2 do
    for y = y_1 to y_2 do 
      b.grid.(x).(y) <- Ship sh
    done
  done


let rec place s l1 l2 b =
  if s="default" then (
    List.fold_left
      (fun _ sh -> try remove sh b with | _ -> ())
      () b.ships;
    List.fold_left
      (fun _ sh -> let def1, def2 = sh.default in
        place_single_ship (string_of_ship sh.name) def1 def2 b)
      () b.ships 
  ) else if s="random" then try 
      List.fold_left (fun _ sh -> try remove sh b with | _ -> ())
        () b.ships;
      List.fold_left
        (fun _ sh ->
           let rand1, rand2 = random_coordinates (sh.size -1) in
           place_single_ship (string_of_ship sh.name) rand1 rand2 b)
        () b.ships 
    with exn -> place "random" "" "" b
  else 
    place_single_ship s l1 l2 b


let place_m_r s (x_1, y_1) (x_2, y_2) b =
  let ship = get_ship s b in 
  overlapping_ship_by_coors ship (x_1, y_1) (x_2, y_2) b.grid;
  if ship.on_board then remove ship b else ();
  ship.on_board <- true;
  ship.orientation <- new_orientation (x_1, y_1) (x_2, y_2);
  for x = x_1 to x_2 do
    for y = y_1 to y_2 do 
      b.grid.(x).(y) <- Ship ship
    done
  done


(** [is_dead s g] is true iff [s] is a sunken ship in the grid [g] (all
    cells have been hit). *)
let is_dead (s:ship) (g : spot array array) = 
  let dead_in_row (s:ship) (r : spot array) = 
    Array.fold_left
      (fun c sp -> c + (if sp = HitShip s then 1 else 0))
      0 r in
  s.size = (Array.fold_left (fun c r -> c + dead_in_row s r) 0 g)


let did_lose b = List.fold_left
    (fun true_so_far s -> true_so_far && is_dead s b.grid) true b.ships

let shoot_helper (x, y) b =
  match b.grid.(x).(y) with 
  | exception Invalid_argument(_)  -> raise InvalidLoc
  | Water -> b.grid.(x).(y) <- ShotWater;
    b.status <- Some ("Your opponent shot "
                      ^(rev_row_col (x, y) |> String.uppercase_ascii)
                      ^" and missed.");
    "It's a miss!", false, false
  | Ship s -> b.grid.(x).(y) <- HitShip s;
    let sh_name = string_of_ship s.name in
    if is_dead s b.grid then (
      (b.status <- Some ("Your opponent sank your "^sh_name^"."));
      ("It's a hit! You sunk your opponent's " ^ sh_name ^ "!"), true, true
    ) 
    else  (
      (b.status <- Some ("Your opponent shot your "^sh_name^"."));
      "It's a hit!", true, false
    )
  | _ -> raise DuplicateShot


let shoot_m_r (x, y) b =
  let _, success, killed = shoot_helper (x, y) b in success, killed

let shoot l b = 
  let message, _, _ = shoot_helper (row_col l) b in message


let setup_status b = 
  let on_board = List.filter (fun s -> s.on_board) b.ships in
  let off_board = List.filter (fun s -> not s.on_board) b.ships in
  "On the board: "
  ^ (long_string_of_ships on_board)
  ^ "\nOff the board: "
  ^ (long_string_of_ships off_board)

let setup_status_m_r b =
  (* let on_board = List.filter (fun s -> s.on_board) b.ships
                 |> List.map (fun s -> (s.name |> string_of_ship, s.size))
     in *)
  let off_board = List.filter (fun s -> not s.on_board) b.ships
                  |> List.map (fun s -> (s.name |> string_of_ship, s.size))
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

let is_part_of_living_ship b (x, y) = 
  let g = b.grid in
  try
    match g.(x).(y) with
    | Ship s | HitShip s -> not (is_dead s g)
    | _ -> false
  with | _ -> false


(** [to_string_grid is_self b] is the grid (string list list) representation
    of board [b]. Represented as seen by the board's player if [is_self] is
    [true], and as seen by opponenets otherwise. *)
let to_string_grid is_self b =
  let rec row_str self g = function
    | [] -> []
    | Water::t -> (if self then "w" else "?")::(row_str self g t)
    | ShotWater::t -> (if self then "x" else "x")::(row_str self g t)
    | (Ship s)::t -> (
        (
          if self then (if s.orientation=Some Vert then "|" else "-")
          else "?"
        )::(row_str self g t)
      )
    | (HitShip s)::t ->
      (if is_dead s g then "#" else (
          if s.orientation=Some Vert then "X|" else "X-"
        ))::(row_str self g t) in
  b.grid |> Array.to_list
  |> List.map (fun r -> row_str is_self b.grid (Array.to_list r))

let string_self (b:t) = to_string_grid true b

let string_other b = to_string_grid false b

let is_unshot b (x,y) = try
    match (b.grid).(x).(y) with
    | Ship _ | Water -> true
    | _ -> false
  with | _ -> false


