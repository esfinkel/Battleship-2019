
exception OffBoard
exception Misaligned
exception WrongLength
exception DuplicateShip
exception OverlappingShips 

exception NoShip
exception DuplicateShot


type ship = {
  name : Command.ship_name;
  size : int
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

let place _ b =
  failwith "unimplemented"

let remove _ b = 
  failwith "unimplemented"

let shoot _ b = 
  failwith "unimplemented"

let status b = 
  failwith "unimplemented"

let complete b = 
  failwith "unimplemented"
