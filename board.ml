
exception OffBoard
exception Misaligned
exception WrongLength
exception DuplicateShip
exception OverlappingShips 

exception NoShip
exception DuplicateShot

exception InvalidLoc


(* should be in command *)
exception InvalidShipName

(** The type [ship_name] represents the name of each ship in the game. *)
type ship_name = Battleship | Cruiser | Carrier | Destroyer | Submarine

type ship = {
  name : ship_name;
  size : int;
  mutable on_board: bool
}

type spot =  Water | ShotWater | Ship of ship | HitShip of ship

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
    | _ -> raise InvalidLoc
  in
  loc |> pull_regex |> tup


(** AF: the record
    { grid = [
        [|
          [|Water; ShotWater|];
          [|Ship s1; HitShip s2|]
        |];
      ships = [s1; s2]
    } represents a board
    where position A1 is water, position A2 is water that has been
    shot, position B1 is a cell of ship s1, and position B2 is a cell
    of ship s2 (and that shell has been shot).

    RI : Once board setup has ended, every ship [s] in [t.ships] 
    appears exactly [s.size] times in [t.grid], either as [Ship s] or
    [HitShip s]). The remaining cells are Water or ShotWater. 
*)


let board_size = 10

type t = {
  grid: spot array array;
  ships: ship list
}

let init_ships () = [
  {
    name=Battleship;
    size=4;
    on_board=false;
  };
  {
    name=Cruiser;
    size=2;
    on_board=false;
  };
  {
    name=Carrier;
    size=5;
    on_board=false;
  };
  {
    name=Destroyer;
    size=3;
    on_board=false;
  };
  {
    name=Submarine;
    size=3;
    on_board=false;
  };
]


let init_board () = {
  grid=Water |> Array.make_matrix board_size board_size;
  ships = init_ships ();
}


(** [ship_of_string str] is the ship with string name [str]. *)
let ship_of_string = function
  | "battleship" -> Battleship
  | "cruiser" -> Cruiser
  | "carrier" -> Carrier
  | "destroyer" -> Destroyer
  | "submarine" -> Submarine
  | _ -> raise InvalidShipName

(** [string_of_ship shp] is the string name of ship [shp]. *)
let string_of_ship = function
  | Battleship -> "battleship"
  | Cruiser -> "cruiser"
  | Carrier -> "carrier"
  | Destroyer -> "destroyer"
  | Submarine -> "submarine"


let get_ship str_name b =
  let sh_name = ship_of_string str_name in
  List.filter (fun s -> s.name=sh_name) b.ships |>
  function 
  | s::[] -> s
  | _ -> raise NoShip

(** [on_board loc] raises [OffBoard] iff [loc] refers to an invalid location 
    on board [b]. *)
let on_board (loc : Command.location) (b : t) =
  let size = board_size in
  match row_col loc with 
  | (r, c) when (0 <= r && r < size) && (0 <= c && c < size) -> ()
  | _ -> raise OffBoard

(** [aligned loc1 loc2] raises [Misaligned] iff [loc1] and [loc2] are not in the
    same row or column. *)
let aligned loc1 loc2 =
  match row_col loc1, row_col loc2 with
  | (r1, c1), (r2, c2) when r1=r2 || c1=c2 -> ()
  | _ -> raise Misaligned

(** [right_length loc1 loc2 s] raises [WrongLength] iff the inclusive distance
    between loc1 and loc2 does not equal the size of [s]. *)
let right_length loc1 loc2 s =
  let (r1, c1), (r2, c2) = row_col loc1, row_col loc2 in
  if r1 = r2 then 
    if (c1 - c2 + 1 = s.size) || (c2 - c1 + 1 = s.size) then ()
    else raise WrongLength
  else if c1 = c2 then 
    if (r1 - r2 + 1 = s.size) || (r2 - r1 + 1 = s.size) then () 
    else raise WrongLength
  else raise WrongLength

(** [duplicate_ship s] raises [DuplicateShip] iff [s] is already 
    present on [b]. *)
let duplicate_ship s = 
  if s.on_board then ()
  else raise DuplicateShip

(** [overlapping_ship s b] is true iff [s] would overlap with a ship
    already present on [b]. *)
let overlapping_ship l1 l2 b =
  let x_1, y_1 = row_col l1 in
  let x_2, y_2 = row_col l2 in 
  for x = x_1 to x_2 do
    for y = y_1 to y_2 do 
      match b.(x).(y) with
      | Water -> ()
      | _ -> raise OverlappingShips 
    done
  done

(* this should call the above functions *)
let place s l1 l2 b =
  let ship = get_ship s b in 
  on_board l1 b;
  on_board l2 b;
  aligned l1 l2;
  right_length l1 l2 ship;
  duplicate_ship ship;
  overlapping_ship l1 l2 b.grid;
  let x_1, y_1 = row_col l1 in
  let x_2, y_2 = row_col l2 in 
  for x = x_1 to x_2 do
    for y = y_1 to y_2 do 
      ship.on_board <- true;
      b.grid.(x).(y) <- Ship ship
    done
  done

(* - OffBoard if [l1] or [l2] is off the game board *)
(* - Misaligned if [l1] and [l2] are not in the same row or column *)
(* - WrongLength if [l1] and [l2] are the wrong distance apart *)
(* - DuplicateShip if the ship has already been placed *)
(* - OverlappingShips if the ship would overlap with a ship already
    present in [b]. *)

let remove _ b = 
  failwith "unimplemented"

let shoot _ b = 
  failwith "unimplemented"

let status b = 
  failwith "unimplemented"

let complete b = 
  failwith "unimplemented"
