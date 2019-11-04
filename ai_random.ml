
type t = {
  board: Board.t;
  name: string;
}

let init () = {
  board = Board.init_board "";
  name = ""
}

let get_board c = c.board

let place_all_ships c = Board.place "default" "" "" c.board

let rec shoot_ship c = 
  let yaxis = Char.chr ((Random.int 9) + 60) in
  let xaxis = string_of_int ((Random.int 9) + 1) in
  Board.shoot (yaxis ^ xaxis)