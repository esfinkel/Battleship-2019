exception OffBoard
exception Misaligned (* boat locations are not same row/column *)
exception WrongLength (* boat locations are wrong distance apart *)
exception OverlappingBoats 
exception NoBoat

type t = unit

type ship = {
  name : Command.ship_name;
  size : int
}

type spot =  Water | ShotWater | Ship of ship | HitShip of ship

let init_board () = ()

let place _ b = ()

let remove _ b = ()

let shoot _ b = ()

let status b = ""
