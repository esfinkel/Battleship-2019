
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

let rec shoot_ship (c:t) b =
  try let st, _, _ =
        Board.shoot (Helpers.random_coor_string (Board.board_size b)) b 
    in st with
  (*BISECT-IGNORE-BEGIN*)
  | _ -> shoot_ship c b
(*BISECT-IGNORE-END*)