exception InvalidShipName

type object_phrase = string list

type location = string 

type command = 
  | Place of object_phrase
  (* | Remove of object_phrase *)
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
  (* | "remove"::boat::[] -> Remove (boat::[]) *)
  | "shoot"::loc::[] -> Shoot (loc::[])
  | "status"::[] -> Status
  | "help"::[] -> Help
  | "quit"::[] -> Quit
  | "ready"::[] -> Ready
  | _ -> raise Malformed

