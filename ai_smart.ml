type t = {
  board: Board.t;
  parity : int option; (* None if you're not using the parity trick *)
}


let init () = {
  board = Board.init_board_default "💻";
  parity = Some (Random.int 2);
}

let init_custom f =
  let b = Board.init_board_from_file "💻" f in
  let parity =
    if Board.min_ship_size b > 1
    then Some (Random.int 2)
    else None in
  {board=b; parity=parity}

let get_board c = c.board

(** [board_size b] is the size of the grid of [b]. *)
let board_size = Board.board_size

(** [get-history b] is a list of hit coordinates on [b] that contain
    living ships. *)
let get_history b =
  (*BISECT-IGNORE-BEGIN*)
  let append ls (elt:Helpers.coor_type) = ls := elt :: (!ls) in
  (*BISECT-IGNORE-END*)
  let full_hist = Board.string_other b
                  |> Array.of_list 
                  |> Array.map (Array.of_list) in
  let is_x s = s="X" || s="X|" || s="X-" in
  let xs = ref [] in
  for i = 0 to (board_size b -1) do
    for j = 0 to (board_size b -1) do
      (*BISECT-IGNORE-BEGIN*)
      if is_x full_hist.(i).(j) then append xs (i, j) else ()
      (*BISECT-IGNORE-END*)
    done
  done;
  !xs

(* **************************** *)

(* ship placement *)

(** [on_board bd a] is true iff a coordinate [(a, a)] would be on [bd]. *)
let on_board bd a = 0 <= a && a < board_size bd

(** [coor_on_board bd coor] is true iff [coor] would be on [board_size]. *)
let coor_on_board bd ((a, b):Helpers.coor_type) =
  on_board bd a && on_board bd b

(** [all_cells_between c1 c2] is all the grid cells (inclusive) between [c1]
    and [c2]. *)
let rec all_cells_between c1 c2 : Helpers.coor_type list = 
  let (i_1, j_1), (i_2, j_2) = Helpers.ordered_coors c1 c2 in
  match (i_1 < i_2, j_1 < j_2) with
  | true, false -> (i_1, j_1)::(all_cells_between (i_1+1, j_1) (i_2, j_2))
  | false, true -> (i_1, j_1)::(all_cells_between (i_1, j_1+1) (i_2, j_2))
  | false, false -> [(i_1, j_1)]
  (*BISECT-IGNORE-BEGIN*) (* error case *)
  | true, true -> failwith "something has gone very wrong"
(*BISECT-IGNORE-END*)

(** [random_placement_coors bd size] is a random pair of coordinates on 
    [bd] that span [size] grid cells. *)
let random_placement_coors bd size : Helpers.coor_type * Helpers.coor_type =
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
    (fun sofar coor ->
       sofar && (List.mem (coor:Helpers.coor_type) off_limits |> not))
    true ship_cells

(** [adjacent_cells coor] is a list of the coordinates that are horizontally
    or vertically adjacent to [coor]. *)
let adjacent_cells ((i, j): Helpers.coor_type) : Helpers.coor_type list =
  (i+1,j)::(i-1,j)::(i,j-1)::(i,j+1)::[]

(** [all_cells_adjacent_to_next_ship ship_cells off_limits] is [off_limits]
    with the addition of all cells that are adjacent to any cell in
    [ship_cells]. *)
let all_cells_adjacent_to_next_ship ship_cells off_limits =
  ship_cells
  |> List.fold_left (fun sofar (coor:Helpers.coor_type) ->
      coor::(adjacent_cells coor) @ sofar
    ) off_limits
  |> List.sort_uniq Stdlib.compare

(** [place b sh c1 c2] places the ship named [sh] on [b] with its ends on
    [c1] and [c2].
    Raises: Same exceptions as Board.place_m_r. *)
let place b sh c1 c2 = Board.place_m_r sh c1 c2 b

(** [place_ship_smartly b name size off_limits dec] is a new list [res]:
    - The ship with [name] and [size] has been placed on [b] such that 
      the ship does not overlap with the cells described by [off_limits].
      In effect, the ship is not directly adjacent to any existing ships.
    - [res] if [off_limits] with the addition of the cells surrounding
      the ship's location.
    - Raises: Same exceptions as Board.place_m_r. *)
let rec place_ship_smartly b name size off_limits dec =
  if dec <= 0 then failwith "Something might be wrong with board params."
  else
    (* pick a two random coors at appropriate distance; order them *)
    let c1, c2 = random_placement_coors b size in
    let c1, c2 = Helpers.ordered_coors c1 c2 in
    (* if anything off_board, recurse *)
    if not (coor_on_board b c1 && coor_on_board b c2)
    then place_ship_smartly b name size off_limits (dec-1)
    else
      (* calculate coors for all ship cells *)
      let ship_cells = all_cells_between c1 c2 in
      (* if any overlap between that and off_limits, recurse *)
      if no_overlap ship_cells off_limits |> not
      then place_ship_smartly b name size off_limits (dec-1)
      (* otherwise add to off_limits and return new off_limits *)
      else
        (place b name c1 c2;
         all_cells_adjacent_to_next_ship ship_cells off_limits)

(** [place_all_ships c] places all ships on the board belonging to [c],
    such that no ships are touching. *)
let place_all_ships c = 
  let to_place = Board.setup_status_m_r c.board in
  try
    List.fold_left
      (fun off_limits (n, s) -> place_ship_smartly c.board n s off_limits 100)
      [] to_place
    |> ignore
  with | _ -> Board.place "random" "" "" c.board


(* **************************** *)


(* shooting *)


(* when shooting randomly *)
(** [random_coors c] is a coordinate [coor], where [coor] is on the board
    belonging to [c] and has parity corresponding to [c.parity]. *)
let random_coors c : Helpers.coor_type =
  let i = (Random.int (board_size c.board)) in 
  let j = (Random.int (board_size c.board)) in 
  (* Use parity to up your chances *)
  match c.parity with
  | None -> (i, j)
  | Some n -> let j = if (i + j) mod 2 = n
                then j
                else (j+1) mod (board_size c.board) in (i, j)


(* Use holistic history, keep track of direction -
   otherwise the same as ai_normal *)

(** [living_coors_adjacent_to_this coor coor_list] is a list of pairs
    [(coor, c2)], where the set of [c2] comes from all of the coordinates in
    [coor_list] that are adjacent to [coor]. Pairs where either coordinate
    is dead are excluded. *)
let rec living_coors_adjacent_to_this
    (coor : Helpers.coor_type)
    (coor_list : Helpers.coor_type list) =
  let is_adjacent (c2) = (List.mem c2 (adjacent_cells coor)) in
  List.filter (is_adjacent) coor_list
  |> List.map (fun (c2) -> (coor, c2))

(** [shoot b coor] shoots coordinate [coor] on [b].
    Raises: Same exceptions as Board.shoot_m_r. *)
let shoot b coor =
  match Board.shoot_m_r (coor) b with
  | _, _ -> ()

(** [shoot_random c b] shoots randomly on board [b]. *)
let rec shoot_random c b = 
  try shoot b (random_coors c)
  with | _ -> shoot_random c b

(** [shoot_from c b targets dec] randomly selects a member of [targets] and
    shoots it on [b]. *)
let rec shoot_from c b targets dec =
  if dec <= 0 then shoot_random c b else 
    try
      let len = List.length targets in
      let target = List.nth targets (Random.int len) in
      shoot b target
    with 
    | _ -> shoot_from c b targets (dec-1)

(** [find_adjacent_xs xs] is a list of all pairs [(c1, c2)], with each [c]
    selected from [xs], such that [c1] is adjacent to [c2] and both
    have been shot (but are not dead). *)
let find_adjacent_xs xs =
  List.fold_left
    (fun sofar coor -> sofar @ (living_coors_adjacent_to_this coor xs))
    [] xs
  |> List.map (fun (a, b) -> Helpers.ordered_coors a b)
  |> List.sort_uniq Stdlib.compare

(** [shoot_find_unknown_on_ends b adjx] is a list of all coordinates [c] on
    [b] such [c] forms a three-point line with the members of one
    of the pairs belonging to [adjx].  *)
let shoot_find_unknown_on_ends b adjx = 
  let ends ((i1, j1), (i2, j2)) = if i1 = i2
    then (* horiz *) [(i1, j1-1); (i2, j2+1)]
    else (* vert *) [(i1-1, j1); (i2+1, j2)] in
  let is_unknown = Board.is_unshot b in
  List.map
    (fun coors ->  coors |> (fun (a, b) -> Helpers.ordered_coors a b) |> ends)
    adjx
  |> List.flatten
  |> List.filter (coor_on_board b)
  |> List.filter is_unknown

(** [nearby b coors] is a list of all coordinates that are on board [b]
    and adjacent to one of the coordinates in [coors] *)
let nearby b coors =
  (List.map adjacent_cells coors
   |> List.flatten)
  |> List.filter (coor_on_board b)


(** [shoot_find_nearby c b] shoots a location on [b] that is likely to be
    part of a ship; if there is no likely location, it shoots randomly. *)
let rec shoot_find_nearby c b = 
  (* look for adjacent X's in history *)
  let hits = get_history b in
  let adjx = find_adjacent_xs hits in
  let targets = shoot_find_unknown_on_ends b adjx in
  let num_targets = List.length targets in
  if num_targets > 0
  (* if you find adjacent x's with ? on at least one end; shoot that ? *)
  (*BISECT-IGNORE-BEGIN*)
  then shoot_from c b targets 100
  else 
    let adj = nearby b hits in
    let adj_questions = adj |> List.filter (Board.is_unshot b)
    in 
    if List.length adj_questions > 0 then 
      shoot_from c b adj_questions 100
      (* if you don't find one, shoot randomly *)
    else shoot_random c b
(*BISECT-IGNORE-END*)

(** [shoot_ship c b] shoots a location on [b] that is likely to be
    part of a ship; if there is no likely location, it shoots randomly. *)
let rec shoot_ship c b = 
  shoot_find_nearby c b; ""

