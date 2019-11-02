
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

let shoot_ship () = ()