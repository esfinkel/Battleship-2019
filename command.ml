type ship_name = Battleship | Cruiser | Carrier | Destroyer | Submarine

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
  | "place"::place_list -> if place_list=[] then raise Malformed
    else Place place_list
  | "remove"::remove_list -> if remove_list=[] then raise Malformed
    else Remove remove_list
  | "shoot"::shoot_list -> if shoot_list=[] then raise Malformed
    else Shoot shoot_list
  | "status"::[] -> Status
  | "help"::[] -> Help
  | "quit"::[] -> Quit
  | "ready"::[] -> Ready
  | _ -> raise Malformed

