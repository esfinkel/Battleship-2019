
exception OffBoard
exception Misaligned
exception WrongLength
exception DuplicateShip
exception OverlappingShips 

exception NoShip
exception DuplicateShot


type ship = {
  name : Command.ship_name;
  size : int;
  on_board: bool
}

type spot =  Water | ShotWater | Ship of ship | HitShip of ship

(** [row_col loc] is the [(row, column)] coordinate pair corresponding
    to [loc]. *)
let row_col (loc : Command.location) : (int*int) =
  failwith "unimplemented"


(** AF: the spot array array
    [
      [|
        [|Water; ShotWater|];
        [|Ship s1; HitShip s2|]
      |]
    ] represents a board
    where position A1 is water, position A2 is water that has been
    shot, position B1 is a cell of ship s1, and position B2 is a cell
    of ship s2 (and that shell has been shot).

    RI : Once board setup has ended, every ship [s] in Command.ship_name 
    appears exactly [n] times in [t] (either as a Ship or HitShip),
    where [n] is the "size" of [s]. The remaining cells are Water or 
    ShotWater. *)

let board_size = 10
type t = spot array array 

let init_board () = Water
                    |> Array.make board_size
                    |> Array.make board_size

(** [on_board loc] raises [OffBoard] iff [loc] refers to an invalid location 
    on board [b]. *)
let on_board (loc : Command.location) (b : t) =
  let size = Array.length b in
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
let overlapping_ship s b =
  failwith "unimplemented"

(* this should call the above functions *)
let place _ b =
  failwith "unimplemented"
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
