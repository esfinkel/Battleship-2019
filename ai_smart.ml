type coor_type = int * int

type t = {
  board: Board.t;
  mutable hit_history: (coor_type*bool) list (* bool represents
                                                whether ship is alive *)
}


let seed_random () = Unix.time () |> int_of_float |> Random.init

let init () =
  let () = seed_random () in
  {
    board = Board.init_board "💻";
    hit_history = [];
  }

let get_board c = c.board

(* **************************** *)

(* ship placement *)

(** [ordered (pos1, pos2)] is [(pos_a, pos_b)], where [pos_a] is the
    lesser of the two and [pos_b] the greater. *)
let ordered ((pos1, pos2) : coor_type * coor_type)
  = if pos1 < pos2 then pos1, pos2 else pos2, pos1

let board_size = 10

let on_board a = 0 <= a && a < board_size
let coor_on_board (a, b) = on_board a && on_board b

(** [all_cells_between c1 c2] is all the grid cells (inclusive) between [c1]
    and [c2]. *)
let rec all_cells_between c1 c2 = 
  let (x_1, y_1), (x_2, y_2) = ordered (c1, c2) in
  match (x_1 < x_2, y_1 < y_2) with
  | true, false -> (x_1, y_1)::(all_cells_between (x_1+1, y_1) (x_2, y_2))
  | false, true -> (x_1, y_1)::(all_cells_between (x_1, y_1+1) (x_2, y_2))
  | false, false -> [(x_1, y_1)]
  | true, true -> failwith "something has gone very wrong"

(** [random_placement_coors size] is a random pair of coordinates that span
    [size] grid cells. *)
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

(** [no_overlap ship_cells off_limits] is true iff there is no overlap
    between [ship_cells] and [off_limits]. *)
let no_overlap ship_cells off_limits =
  List.fold_left
    (fun sofar coor -> sofar && (List.mem coor off_limits |> not))
    true ship_cells

(** [adjacent_cells coor] is a list of the coordinates that are horizontally
    or vertically adjacent to [coor]. *)
let adjacent_cells (x, y) = (x+1,y)::(x-1,y)::(x,y-1)::(x,y+1)::[]

(** [all_cells_adjacent_to_next_ship ship_cells off_limits] is [off_limits]
    with the addition of the cells that are adjacent to any cell in
    [ship_cells]. *)
let all_cells_adjacent_to_next_ship ship_cells off_limits =
  List.fold_left (fun sofar coor ->
      coor::(adjacent_cells coor) @ sofar
    ) off_limits ship_cells |> List.sort_uniq Stdlib.compare

(** [place b sh c1 c2] is [()]; the ship named [sh] has been placed
    on [b] with its ends on [c1] and [c2]. *)
let place b sh c1 c2 = Board.place_m_r sh c1 c2 b

(** - [place_ship_smartly b name size off_limits] is a new list [res].
    - The ship with [name] and [size] has been placed on [b] such that 
      the ship does not overlap with the cells described by [off_limits].
      In effect, the ship is not touching any existing ships.
    - [res] if [off_limits] with the addition of the cells surrounding
      the ship's location. *)
let rec place_ship_smartly b name size off_limits =
  (* pick a random coor, and a diff random at appropriate distance *)
  let new_coors = random_placement_coors size in
  (* order them *)
  let c1, c2 = ordered new_coors in
  (* if anything off_board, recurse *)
  if not (coor_on_board c1 && coor_on_board c2)
  then place_ship_smartly b name size off_limits
  else
    (* calculate coors for all ship cells *)
    let ship_cells = all_cells_between c1 c2 in
    (* if any overlap between that and off_limits, recurse *)
    if no_overlap ship_cells off_limits |> not
    then place_ship_smartly b name size off_limits
    (* otherwise add to off_limits and return new off_limits *)
    else
      (place b name c1 c2;
       all_cells_adjacent_to_next_ship ship_cells off_limits)

(** [place_all_ships c] is [()]. All ships have been placed on the
    board belonging to [c], such that no ships are touching. *)
let place_all_ships c = 
  let to_place = Board.setup_status_m_r c.board in
  List.fold_left
    (fun off_limits (n, s) -> place_ship_smartly c.board n s off_limits)
    [] to_place
  |> ignore


(* **************************** *)


(* shooting *)

(* when shooting randomly *)
(* Fire at the center of the board *)
(* Use parity to up your chances *)
(* Move away when you have two misses in the same segment *)

let random_coors () =
  let x = (Random.int board_size) in 
  let y = (Random.int board_size) in 
  (x, y)





(* when shooting with history, keep track of direction -
   otherwise the same as ai_normal *)

let rec coors_adjacent_to_this
    (coor : coor_type)
    (coor_list : (coor_type*bool) list) =
  let is_adjacent (c,b) = (List.mem c (adjacent_cells coor) && b) in
  List.filter (is_adjacent) coor_list
  |> List.map (fun (c2, b) -> (coor, c2))

let shoot c b coor =
  match Board.shoot_m_r (coor) b with
  | true, is_dead -> c.hit_history <- (coor, is_dead)::c.hit_history; ""
  | _ -> ""

let rec shoot_random c b = 
  print_endline "shooting randomly";
  try () |> random_coors |> shoot c b
  with | _ -> shoot_random c b

let rec shoot_from c b targets =
  print_endline "shooting from list";
  try
    let len = List.length targets in
    let target = List.nth targets (Random.int len) in
    shoot c b target
  with 
  | _ -> shoot_from c b targets

let rec find_adjacent_xs ls = function
  | [] -> ls
  | (c, is_alive)::t -> find_adjacent_xs (ls @ (coors_adjacent_to_this c t)) t

let shoot_find_unknown_on_ends b adjx = 
  let ends ((x1, y1), (x2, y2)) = if x1 = x2
    then (* horiz *) [(x1, y1-1); (x2, y2+1)]
    else (* vert *) [(x1-1, y1); (x2+1, y2)] in
  let is_unknown = Board.is_unshot b in
  List.fold_left (fun ls coors -> ls @ (coors |> ordered |> ends)) [] adjx
  |> List.filter coor_on_board
  |> List.filter is_unknown

let update_history c =
  c.hit_history <- (
    List.map (fun (coor, _) ->
        (coor, Board.is_part_of_living_ship c.board coor)
      ) c.hit_history
  )

let rec shoot_find_nearby c b = 
  (* update all the is_dead values in this hit_history *)
  update_history c;
  (* look for adjacent X's in history *)
  let adjx = find_adjacent_xs [] c.hit_history in
  let targets = shoot_find_unknown_on_ends b adjx in
  let num_targets = List.length targets in
  if num_targets > 0
  (* if you find adjacent x's with ? on at least one end; shoot that ? *)
  then ((print_endline "looking at ends"); let res = shoot_from c b targets in update_history c; res)

  (* otherwise, shoot a ? next to any X *)
  (* if you don't find one, shoot randomly *)
  else 
    let adj_questions = all_cells_adjacent_to_next_ship targets [] 
                        |> List.filter coor_on_board
                        |> List.filter (Board.is_unshot b)

    in 
    let res = (
      if List.length adj_questions > 0 then ((print_endline "looking at all adjacent"); shoot_from c b adj_questions)
      else shoot_random c b
    ) in
    (* finally, update all the is_dead values in this hit_history *)
    update_history c; res


let rec shoot_ship c b = 
  if c.hit_history = [] then
    shoot_random c b else
    shoot_find_nearby c b

(* **************************** *)
