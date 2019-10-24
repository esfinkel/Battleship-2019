type ship_name = Battleship | Cruiser | Carrier | Destroyer | Submarine

type ship = {
  name : ship_name;
  size : int
}

type object_phrase = string list

type location = string 

type command = 
  | Place of object_phrase
  | Remove of object_phrase
  | Shoot of object_phrase
  | Status
  | Help 

exception Empty

exception Malformed

let parse _ = Help

