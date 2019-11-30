
type t = {
  board: Board.t;
}


let init () = {
  board = Board.init_board_default "💻";
}

let get_board c = c.board

let place_all_ships c = Board.place "random" "" "" c.board

let random_coors b =
  let yaxis = Char.chr ((Random.int (Board.board_size b)) + 65)
              |> String.make 1 in
  let xaxis = string_of_int ((Random.int (Board.board_size b)) + 1) in
  yaxis ^ xaxis


let rec shoot_ship c b = try Board.shoot (random_coors b) b |> fst with
  | _ -> shoot_ship c b
