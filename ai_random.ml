
type t = {
  board: Board.t;
}


let init () = {
  board = Board.init_board_default "💻";
}

let init_custom f = {
  board = Board.init_board_from_file "💻" f;
}


let get_board c = c.board

let place_all_ships c = Board.place "random" "" "" c.board

let random_coors b =
  let yaxis = Char.chr ((Random.int (Board.board_size b)) + 65)
              |> String.make 1 in
  let xaxis = string_of_int ((Random.int (Board.board_size b)) + 1) in
  yaxis ^ xaxis

let get_fst (x, _, _) = x

let rec shoot_ship c b = try Board.shoot (random_coors b) b |> get_fst with
  | _ -> shoot_ship c b
