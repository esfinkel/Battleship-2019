type ship_name = Battleship | Cruiser | Carrier | Destroyer | Submarine


exception InvalidShipName

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


type object_phrase = string list

type location = string 

type command = 
  | Place of object_phrase
  | Remove of object_phrase
  | Shoot of object_phrase
  | Status
  | Help 
  | Quit
  | Ready

exception Empty

exception Malformed

let parse str = 
  str 
  |> String.split_on_char ' ' 
  |> List.filter (fun x -> x <> "")
  |> function
  | [] -> raise Empty
  | "place"::boat::"on"::l1::l2::[] -> Place (boat::l1::l2::[])
  | "remove"::boat::[] -> Remove (boat::[])
  | "shoot"::loc::[] -> Shoot (loc::[])
  | "status"::[] -> Status
  | "help"::[] -> Help
  | "quit"::[] -> Quit
  | "ready"::[] -> Ready
  | _ -> raise Malformed

