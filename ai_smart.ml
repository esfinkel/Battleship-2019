(** [coor_type] is a type for values representing grid coordinates. *)
type coor_type = int * int

type t = {
  board: Board.t;
  (* mutable hit_history: coor_type list; *)
  parity : int option; (* None if you're not using the parity trick *)
}


let init () = {
  board = Board.init_board_default "💻";
  (* hit_history = []; *)
  parity = Some (Random.int 2);
}

let get_board c = c.board

let board_size = Board.board_size

let get_history b =
  let append ls (elt:coor_type) = ls := elt :: (!ls) in
  let full_hist = Board.string_other b
                  |> Array.of_list 
                  |> Array.map (Array.of_list) in
  let is_x s = s="X" || s="X|" || s="X-" in
  let xs = ref [] in
  for i = 0 to (board_size b -1) do
    for j = 0 to (board_size b -1) do
      if is_x full_hist.(i).(j) then append xs (i, j) else ()
    done
  done;
  !xs

(* **************************** *)

(* ship placement *)

(** [ordered (pos1, pos2)] is [(pos_a, pos_b)], where [pos_a] is the
    lesser of the two and [pos_b] the greater. *)
let ordered ((pos1, pos2) : coor_type * coor_type)
  = if pos1 < pos2 then pos1, pos2 else pos2, pos1

(** [on_board a] is true iff a coordinate with i=[a] would be on a square
    grid with size [board_size]. *)
let on_board bd a = 0 <= a && a < board_size bd

(** [coor_on_board coor] is true iff [coor] would be on a square grid with
    size [board_size]. *)
let coor_on_board bd (a, b) = on_board bd a && on_board bd b

(** [all_cells_between c1 c2] is all the grid cells (inclusive) between [c1]
    and [c2]. *)
let rec all_cells_between c1 c2 = 
  let (i_1, j_1), (i_2, j_2) = ordered (c1, c2) in
  match (i_1 < i_2, j_1 < j_2) with
  | true, false -> (i_1, j_1)::(all_cells_between (i_1+1, j_1) (i_2, j_2))
  | false, true -> (i_1, j_1)::(all_cells_between (i_1, j_1+1) (i_2, j_2))
  | false, false -> [(i_1, j_1)]
  | true, true -> failwith "something has gone very wrong"

(** [random_placement_coors size] is a random pair of coordinates that span
    [size] grid cells. *)
let random_placement_coors bd size =
  let c1_i = (Random.int (board_size bd)) in 
  let c1_j = (Random.int (board_size bd)) in 
  let c1 = (c1_i, c1_j) in
  let dir = Random.int 4 in
  match dir with
  | 0 -> c1, (c1_i, c1_j+size-1) (* up *)
  | 1 -> c1, (c1_i+size-1, c1_j) (* right *)
  | 2 -> c1, (c1_i, c1_j-size+1) (* down *)
  | _ -> c1, (c1_i-size+1, c1_j) (* left *)

(** [no_overlap ship_cells off_limits] is [true] iff there is no overlap
    between [ship_cells] and [off_limits]. *)
let no_overlap ship_cells off_limits =
  List.fold_left
    (fun sofar coor -> sofar && (List.mem coor off_limits |> not))
    true ship_cells

(** [adjacent_cells coor] is a list of the coordinates that are horizontally
    or vertically adjacent to [coor]. *)
let adjacent_cells (i, j) = (i+1,j)::(i-1,j)::(i,j-1)::(i,j+1)::[]

(** [all_cells_adjacent_to_next_ship ship_cells off_limits] is [off_limits]
    with the addition of all cells that are adjacent to any cell in
    [ship_cells]. *)
let all_cells_adjacent_to_next_ship ship_cells off_limits =
  ship_cells
  |> List.fold_left (fun sofar coor ->
      coor::(adjacent_cells coor) @ sofar
    ) off_limits
  |> List.sort_uniq Stdlib.compare

(** [place b sh c1 c2] places the ship named [sh] on [b] with its ends on
    [c1] and [c2]. *)
let place b sh c1 c2 = Board.place_m_r sh c1 c2 b

(** - [place_ship_smartly b name size off_limits] is a new list [res]
    (see end).
    - The ship with [name] and [size] has been placed on [b] such that 
      the ship does not overlap with the cells described by [off_limits].
      In effect, the ship is not touching any existing ships.
    - [res] if [off_limits] with the addition of the cells surrounding
      the ship's location. *)
let rec place_ship_smartly b name size off_limits =
  (* pick a random coor, and a diff random at appropriate distance *)
  let new_coors = random_placement_coors b size in
  (* order them *)
  let c1, c2 = ordered new_coors in
  (* if anything off_board, recurse *)
  if not (coor_on_board b c1 && coor_on_board b c2)
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

(** [place_all_ships c] places all ships on the board belonging to [c],
    such that no ships are touching. *)
let place_all_ships c = 
  let to_place = Board.setup_status_m_r c.board in
  List.fold_left
    (fun off_limits (n, s) -> place_ship_smartly c.board n s off_limits)
    [] to_place
  |> ignore


(* **************************** *)


(* shooting *)


(* when shooting randomly *)
(** [random_coors c] is a tuple [(i, j)] where (i, j) is on the board
    belonging to [c], and has parity corresponding to [c.parity]. *)
let random_coors c =
  let i = (Random.int (board_size c.board)) in 
  let j = (Random.int (board_size c.board)) in 
  (* Use parity to up your chances *)
  match c.parity with
  | None -> (i, j)
  | Some n -> let j = if (i + j) mod 2 = n
                then j
                else (j+1) mod (board_size c.board) in (i, j)
(* may not implement: *)
(* Fire at the center of the board *)
(* Move away when you have two misses in the same segment *)





(* when shooting with history, keep track of direction -
   otherwise the same as ai_normal *)

(** [living_coors_adjacent_to_this coor coor_list] is a list of pairs
    [(coor, c2)] where the [c2] come from all of the coordinates in
    [coor_list] that are adjacent to [coor]. Pairs where either coordinate
    is dead are excluded. *)
let rec living_coors_adjacent_to_this
    (coor : coor_type)
    (coor_list : (coor_type) list) =
  let is_adjacent (c2) = (List.mem c2 (adjacent_cells coor)) in
  List.filter (is_adjacent) coor_list
  |> List.map (fun (c2) -> (coor, c2))

(** [shoot b coor] shoots coordinate [coor] on [b]. *)
let shoot b coor =
  match Board.shoot_m_r (coor) b with
  | _ -> ()

(** [shoot_random c b] shoots randomly on board [b]. *)
let rec shoot_random c b = 
  (* print_endline "shooting randomly"; *)
  try shoot b (random_coors c)
  with | _ -> shoot_random c b

(** [shoot_from b targets] randomly selects a member of [targets] and
    shoots it on [b]. *)
let rec shoot_from b targets =
  try
    let len = List.length targets in
    let target = List.nth targets (Random.int len) in
    shoot b target
  with 
  | _ -> shoot_from b targets

(** [find_adjacent_xs xs] is a list of all pairs [(m, n)] such
    that [m] is adjacent to [n] and both have been shot (but are not dead). *)
let find_adjacent_xs xs =
  (* let xs = xs
           |> List.filter
             (fun coor -> Board.is_part_of_living_ship b coor) in *)
  List.fold_left
    (fun sofar coor -> sofar @ (living_coors_adjacent_to_this coor xs))
    [] xs
  |> List.map ordered
  |> List.sort_uniq Stdlib.compare

(** [shoot_find_unknown_on_ends b adjx] is a list of all coordinates [c] on
    [b] such [c] forms a three-point line with the members of one
    of the pairs belonging to [adjx].  *)
let shoot_find_unknown_on_ends b adjx = 
  let ends ((i1, j1), (i2, j2)) = if i1 = i2
    then (* horiz *) [(i1, j1-1); (i2, j2+1)]
    else (* vert *) [(i1-1, j1); (i2+1, j2)] in
  let is_unknown = Board.is_unshot b in
  List.map (fun coors ->  coors |> ordered |> ends) adjx
  |> List.flatten
  |> List.filter (coor_on_board b)
  |> List.filter is_unknown

(** [nearby coors] is a list of all coordinates that are on the board
    and adjacent to one of the coordinates in [coors] *)
let nearby b coors =
  (List.map adjacent_cells coors
   |> List.flatten)
  |> List.filter (coor_on_board b)


(* let print_coor (x, y) = Helpers.rev_row_col (x,y) |> print_string *)


(* let print_coor_list ls = 
   let rec print_l_h = function
    | [] -> ()
    | coor::t -> print_coor coor; print_string "; "; print_l_h t
   in print_string "[ "; print_l_h ls; print_endline "]" *)

(* let print_coor_pair_lst ls =
   let rec print_l_h = function
    | [] -> ()
    | (c1, c2)::t -> print_string "("; print_coor c1; print_string ", ";
    print_coor c2; print_string ")"; print_string "; "; print_l_h t
   in print_string "[ "; print_l_h ls; print_endline "]" *)

(** [shoot_find_nearby c b] shoots a location on [b] that is likely to be
    part of a ship; if there is no likely location, it shoots randomly. 
    It then updates the hit history of [c]. *)
let rec shoot_find_nearby c b = 
  (* (print_string "hit history: "; print_coor_bool_list c.hit_history ;
     print_newline () ); *)
  (* look for adjacent X's in history *)
  let hits = get_history b in
  let adjx = find_adjacent_xs hits in
  (* (print_string "adjx: "; print_coor_pair_lst adjx; print_newline () ); *)
  let targets = shoot_find_unknown_on_ends b adjx in
  (* (print_string "end targets: ";print_coor_list targets); *)
  let num_targets = List.length targets in
  if num_targets > 0
  (* if you find adjacent x's with ? on at least one end; shoot that ? *)
  then (
    (* (print_endline "looking at ends"); *)
    shoot_from b targets)
  (* otherwise, shoot a ? next to any X *)
  else 
    (* let hits =
       (* List.filter (fun (_, b) -> b) *)
       history
       |> List.filter (Board.is_part_of_living_ship b) 
       in    *)
    (* ;List.fold_left (fun ls (coor, b) -> if b then coor::ls else ls)
       [] c.hit_history in *)
    (* (print_string "\nliving hits: "; print_coor_list hits;
       print_newline () ); *)
    let adj = nearby b hits in
    (* (print_string "adj: "; List.length adj |> print_int;
       print_newline () ); *)
    let adj_questions = adj |> List.filter (Board.is_unshot b)
    in 
    (* (print_string "adjq: "; List.length adj_questions |> print_int;
       print_newline () ); *)
    if List.length adj_questions > 0 then (
      (* (print_endline "looking at all adjacent"); *)
      shoot_from b adj_questions)
    (* if you don't find one, shoot randomly *)
    else shoot_random c b

(** [shoot_ship c b] shoots a location on [b] that is likely to be
    part of a ship; if there is no likely location, it shoots randomly. 
    It then updates the hit history of [c]. *)
let rec shoot_ship c b = 
  shoot_find_nearby c b; ""
(* (if c.hit_history = [] then
   shoot_random c b else *)

(* **************************** *)
