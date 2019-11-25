
type t = {
  board: Board.t;
}


let init () = {
  board = Board.init_board "💻";
}

let get_board c = c.board

let place_all_ships c = Board.place "random" "" "" c.board

let random_coors () =
  let yaxis = Char.chr ((Random.int 10) + 65) |> String.make 1 in
  let xaxis = string_of_int ((Random.int 10) + 1) in
  yaxis ^ xaxis

let get_fst tup = 
  match tup with 
  | (x, y, z) -> x

let rec shoot_ship c b = try Board.shoot (random_coors ()) b |> get_fst with
  | _ -> shoot_ship c b
