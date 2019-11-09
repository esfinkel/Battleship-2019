
type t = {
  board: Board.t;
  mutable history: (Command.location list)
}

let seed_random () = Unix.time () |> int_of_float |> Random.init

let init () =
  let () = seed_random () in
  {
    board = Board.init_board "💻";
    history = [];
  }

let get_board c = c.board

(* **************************** *)

(* ship placement *)

let ordered (pos1, pos2) = if pos1 < pos2 then pos1, pos2 else pos2, pos1

let board_size = 10

let on_board a = 0 <= a && a < board_size
let coor_on_board (a, b) = on_board a && on_board b

let rec all_cells_between c1 c2 = 
  let (x_1, y_1), (x_2, y_2) = ordered (c1, c2) in
  match (x_1 < x_2, y_1 < y_2) with
  | true, false -> (x_1, y_1)::(all_cells_between (x_1+1, y_1) (x_2, y_2))
  | false, true -> (x_1, y_1)::(all_cells_between (x_1, y_1+1) (x_2, y_2))
  | false, false -> [(x_1, y_1)]
  | true, true -> failwith "something has gone very wrong"

let random_placement_coors size =
  let c1_x = (Random.int board_size) in 
  let c1_y = (Random.int board_size) in 
  let c1 = (c1_x, c1_y) in
  let dir = Random.int 4 in
  match dir with
  | 0 -> c1, (c1_x, c1_y+size-1) (* up *)
  | 1 -> c1, (c1_x+size-1, c1_y) (* right *)
  | 2 -> c1, (c1_x, c1_y-size+1) (* down *)
  | _ -> c1, (c1_x-size+1, c1_y) (* left *)

let no_overlap ship_cells off_limits =
  List.fold_left
    (fun sofar coor -> sofar && (List.mem coor off_limits |> not))
    true ship_cells

let new_off_limits ship_cells off_limits =
  List.fold_left (fun sofar (x,y) ->
      (x,y)::(x+1,y)::(x-1,y)::(x,y-1)::(x,y+1)::sofar
    ) off_limits ship_cells |> List.sort_uniq Stdlib.compare

let place b sh c1 c2 = Board.place_m_r sh c1 c2 b

(* Space the ships out so that they do not touch *)
let rec place_ship_smartly (c:t) b name size off_limits =
  (* pick a random coor, and a diff random at appropriate distance *)
  let new_coors = random_placement_coors size in
  (* order them *)
  let c1, c2 = ordered new_coors in
  (* if anything off_board, recurse *)
  if not (coor_on_board c1 && coor_on_board c2)
  then place_ship_smartly c b name size off_limits
  else
    (* calculate coors for all ship cells *)
    let ship_cells = all_cells_between c1 c2 in
    (* if any overlap between that and off_limits, recurse *)
    if no_overlap ship_cells off_limits |> not
    then place_ship_smartly c b name size off_limits
    (* otherwise add to off_limits and return new off_limits *)
    else
      (place b name c1 c2;
       new_off_limits ship_cells off_limits)


let place_all_ships c = 
  let to_place = Board.setup_status_m_r c.board in
  List.fold_left
    (fun off_limits (n, s) -> place_ship_smartly c c.board n s off_limits)
    [] to_place
  |> ignore


let rec shoot c op = ()

(* **************************** *)


(* shooting *)

(* when shooting randomly *)
(* Fire at the center of the board *)
(* Use parity to up your chances *)
(* Move away when you have two misses in the same segment *)


(* when shooting with history, keep track of direction -
   otherwise the same as ai_normal *)


(* **************************** *)



let random_coors () =
  let yaxis = Char.chr ((Random.int 10) + 65) |> String.make 1 in
  let xaxis = string_of_int ((Random.int 10) + 1) in
  yaxis ^ xaxis


let rec shoot_ship c b = try Board.shoot (random_coors ()) b with
  | _ -> shoot_ship c b
