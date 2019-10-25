
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

type t = unit

(* type t = spot array array  *)



let init_board () = ()

let place _ b = ()

let remove _ b = ()

let shoot _ b = ()

let status b = ""
