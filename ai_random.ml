
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

let get_fst (x, _, _) = x

(*Because of the nature of the random function, these functions 
  cannot be tested*)
(*BISECT-IGNORE-BEGIN*)
let place_all_ships c = Board.place "random" "" "" c.board

let rec shoot_ship c b =
  try Board.shoot (Helpers.random_coor_string (Board.board_size b)) b
      |> get_fst with
  | _ -> shoot_ship c b
(*BISECT-IGNORE-END*)
