type ship_name = Battleship | Cruiser | Carrier | Destroyer | Submarine

type ship = {
  name : ship_name;
  size : int
}

type location = string 

type command = 
  | Place of ship_name * location * location
  | Remove of ship_name
  | Shoot of location
  | Status
  | Help 

exception Empty

exception Malformed

val parse : string -> command 


