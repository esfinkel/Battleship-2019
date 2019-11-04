
type t = {
  board: Board.t;
  name: string;
}

let seed_random () = Unix.time () |> int_of_float |> Random.init

let init () =
  let () = seed_random () in
  {
    board = Board.init_board "";
    name = ""
  }

let get_board c = c.board

let place_all_ships c = Board.place "default" "" "" c.board


let rec shoot_ship b = try Board.shoot "a1" b with
  | _ -> shoot_ship b