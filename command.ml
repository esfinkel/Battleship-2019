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
  let total_list = str 
                   |> String.split_on_char ' ' 
                   |> List.filter (fun x -> x <> "") in 
  if total_list = [] then raise Empty 
  else 
    let first = List.nth total_list 0 in 
    if first = "place" then 
      let place_list = (List.filter (fun x -> x <> "place") total_list) in 
      if place_list = [] then raise Malformed 
      else Place place_list
    else if first = "remove" then 
      let remove_list = (List.filter (fun x -> x <> "remove") total_list) in 
      if remove_list = [] then raise Malformed
      else Remove remove_list
    else if first = "shoot" then 
      let shoot_list = (List.filter (fun x -> x <> "shoot") total_list) in 
      if shoot_list = [] then raise Malformed 
      else Shoot shoot_list
    else if first = "status" then 
      if List.filter (fun x -> x <> "status") total_list = [] then Status
      else raise Malformed
    else if first = "help" then 
      if List.filter (fun x -> x <> "help") total_list = [] then Help
      else raise Malformed
    else if first = "quit" then 
      if List.filter (fun x -> x <> "quit") total_list = [] then Quit
      else raise Malformed
    else if first = "ready" then 
      if List.filter (fun x -> x <> "ready") total_list = [] then Ready
      else raise Malformed
    else raise Malformed 

