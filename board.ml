Random.self_init();
exception OffBoard
exception Misaligned
exception WrongLength
exception OverlappingShips 
exception NoShip
exception DuplicateShot
exception InvalidLoc
exception InvalidShipName

(** The type [ship_name] represents the name of each ship in the game. *)
type ship_name = Battleship | Cruiser | Carrier | Destroyer | Submarine

(** The abstract type of values representing a ship. *)
type ship = {
  name : ship_name;
  size : int;
  mutable on_board: bool;
  default : (Command.location * Command.location)
}

(** The abstract type of values representing a grid spot. *)
type spot =  Water | ShotWater | Ship of ship | HitShip of ship

(** [row_col loc] is the [(row, column)] coordinate pair corresponding
    to [loc]. *)
let row_col (loc : Command.location) : (int*int) =
  let r = Str.regexp "\\([a-z]\\)\\([0-9]+\\)" in
  let pull_regex s =
    Str.global_replace r "\\1 \\2" (String.lowercase_ascii s)
    |> String.split_on_char ' ' in
  let index (c:char) : int = Char.code c - 97 in
  let tup = function
    | letter::number::[] -> (
        String.get letter 0 |> index,
        Stdlib.int_of_string number - 1
      )
    | _ -> raise InvalidLoc
  in
  loc |> pull_regex |> tup

(** [ordered l1 l2] is [(l1_x, l1_y), (l2_x, l2_y)], the coordinates of
    location [l1] and location [l2] respectively, except they are swapped
    if given in the reverse order. *)
let ordered l1 l2 = 
  let pos1, pos2 = (row_col l1), (row_col l2) in
  if fst pos1 < fst pos2 || snd pos1 < snd pos2
  then pos1, pos2
  else pos2, pos1

(** AF: the record
    [{ grid = [
        [|
          [|Water; ShotWater|];
          [|Ship s1; HitShip s2|]
        |];
      ships = [s1; s2];
      player_name = "p1";
      status = Some "opponent missed"
    }]
    represents a board where the player's name is "p1", the player's
    opponent missed their last shot, position A1 is water, position A2
    is water that has been shot, position B1 is a cell of ship s1, and
    position B2 is a cell of ship s2 (and that shell has been shot).

    RI : Once board setup has ended, every ship [s] in [t.ships] 
    appears exactly [s.size] times in [t.grid], either as [Ship s] or
    [HitShip s]). The remaining cells are [Water] or [ShotWater]. 
*)
type t = {
  grid: spot array array;
  ships: ship list;
  player_name: string;
  mutable status: string option;
}

let board_size = 10

(* Used for getting random letters. (Didn't have to hardcode this but figured
   it was easier since our gameboard is always the same size.) *)
let lst = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

(** [choose_random_letter ()] is a random letter represented as a string from 
    the first ten letters of the alphabet in the list [lst] defined above. *)
let choose_random_letter () =  
  List.nth lst (Random.int 10)

(** [random_coordinates size] is a set of valid random locations used for 
    randomly placing a ship of size [size]. *)
let random_coordinates size : Command.location * Command.location = 
  (* horizontal ship placements with probability 0.5 *)
  if Random.bool() then 
    let letter = choose_random_letter () in
    let number = (Random.int 10) + 1 in 
    if ((number + size) < board_size) then begin
      ((letter ^ string_of_int(number)), 
       (letter ^ string_of_int(number + size))) end
    else begin
      ((letter ^ string_of_int(number)), 
       (letter ^ string_of_int(number - size))) end
  else (* vertical ship placements with probability 0.5 *)
    let letter_num = (Random.int 10) in 
    let number = string_of_int((Random.int 10) + 1) in 
    if ((letter_num + size) < board_size) then begin
      ((List.nth lst letter_num) ^ number), 
      ((List.nth lst (letter_num + size)) ^ number) end
    else begin 
      (((List.nth lst letter_num) ^ number), 
       ((List.nth lst (letter_num - size)) ^ number)) end

(** [init_ships ()] is a list of all ships in gameplay, with the appropriate
    names and sizes, [on_board] set to [false], and [default] location values
    hardcoded. *)
let init_ships () = [
  {
    name=Battleship;
    size=4;
    on_board=false;
    default=("a1", "a4")
  };
  {
    name=Cruiser;
    size=2;
    on_board=false;
    default=("b1", "b2")
  };
  {
    name=Carrier;
    size=5;
    on_board=false;
    default=("d2", "d6")
  };
  {
    name=Destroyer;
    size=3;
    on_board=false;
    default=("e2", "g2")
  };
  {
    name=Submarine;
    size=3;
    on_board=false;
    default=("f4", "f6")
  };
]


let init_board player_name = {
  grid=Water |> Array.make_matrix board_size board_size;
  ships = init_ships ();
  player_name= player_name;
  status=None;
}

let player_name b = b.player_name

(** [ship_of_string str] is the ship_name with string name [str]. *)
let ship_of_string = function
  | "battleship" -> Battleship
  | "cruiser" -> Cruiser
  | "carrier" -> Carrier
  | "destroyer" -> Destroyer
  | "submarine" -> Submarine
  | _ -> raise InvalidShipName

(** [string_of_ship shp] is the string name of ship_name [shp]. *)
let string_of_ship = function
  | Battleship -> "battleship"
  | Cruiser -> "cruiser"
  | Carrier -> "carrier"
  | Destroyer -> "destroyer"
  | Submarine -> "submarine"

(** [long_string_of_ships shp_lst] is a long-form string description
    of [shp_lst]. *)
let long_string_of_ships shp_lst = 
  let to_string sh = 
    (string_of_ship sh.name)^" (length "^(sh.size |> string_of_int)^")" in
  if List.length shp_lst = 0 then "None" else
    String.concat "; " (List.map to_string shp_lst) 

(** [get_ship str_name b] is the ship on [b] with string name [str_name]. *)
let get_ship str_name b =
  let sh_name = ship_of_string str_name in
  List.filter (fun s -> s.name=sh_name) b.ships |>
  function 
  | s::[] -> s
  | _ -> raise NoShip

(** [on_board loc b] raises [OffBoard] iff [loc] refers to an invalid
    location  on board [b]. *)
let on_board (loc : Command.location) (b : t) =
  let size = board_size in
  match row_col loc with 
  | (r, c) when (0 <= r && r < size) && (0 <= c && c < size) -> ()
  | _ -> raise OffBoard

(** [check_alignment_and_length loc1 loc2 s] raises [WrongLength] iff the
    inclusive distance between loc1 and loc2 does not equal the size of [s],
    or [Misaligned] iff [loc1] and [loc2] are not in the same row or
    column. *)
let check_alignment_and_length loc1 loc2 s =
  let (r1, c1), (r2, c2) = row_col loc1, row_col loc2 in
  if r1 = r2 then 
    if (c1 - c2 + 1 = s.size) || (c2 - c1 + 1 = s.size) then ()
    else raise WrongLength
  else if c1 = c2 then 
    if (r1 - r2 + 1 = s.size) || (r2 - r1 + 1 = s.size) then () 
    else raise WrongLength
  else raise Misaligned

(** [overlapping_ship l1 l2 b] is true iff there are any ships present in
    the span from [l1] to [l2] on [b]. *)
let overlapping_ship l1 l2 b =
  let ((x_1, y_1), (x_2, y_2)) = ordered l1 l2 in  
  for x = x_1 to x_2 do
    for y = y_1 to y_2 do 
      match b.(x).(y) with
      | Water -> ()
      | _ -> raise OverlappingShips 
    done
  done


(** [remove n b] is [()]. If a ship with name [n] was present in [b], it
    has been removed, and the cells are replaced with Water.
    Raises:
    - NoShip if that ship has not been placed. *)
let remove sh b = 
  let remove_from_row r s = 
    Array.iteri (fun j spot -> if (spot = Ship s) then 
                    r.(j) <- Water else ()) r in
  if sh.on_board 
  then (sh.on_board <- false; 
        Array.iter (fun r -> remove_from_row r sh) b.grid;)
  else raise NoShip

let rec place s l1 l2 b =
  if s="default" then (
    List.fold_left (fun _ sh -> try remove sh b with | _ -> ()) () b.ships;
    List.fold_left
      (fun _ sh -> let def1, def2 = sh.default in
        place (string_of_ship sh.name) def1 def2 b) () b.ships 
  ) else if s="random" then place_random b
  else 
    let ship = get_ship s b in 
    let ((x_1, y_1) as coors_1, (x_2, y_2) as coors_2) = ordered l1 l2 in 
    on_board l1 b;
    on_board l2 b;
    check_alignment_and_length l1 l2 ship;
    if ship.on_board then remove ship b else ();
    overlapping_ship l1 l2 b.grid;
    for x = x_1 to x_2 do
      for y = y_1 to y_2 do 
        ship.on_board <- true;
        b.grid.(x).(y) <- Ship ship
      done
    done
and place_random b = 
  try 
    List.fold_left (fun _ sh -> try remove sh b with | _ -> ()) () b.ships;
    List.fold_left
      (fun _ sh -> let rand1, rand2 = random_coordinates (sh.size -1) in
        place (string_of_ship sh.name) rand1 rand2 b) () b.ships 
  with exn -> place_random b

(** [is_dead s g] is true iff [s] is a sunken ship in the grid [g] (all
    cells have been hit). *)
let is_dead (s:ship) (g : spot array array) = 
  let dead_in_row (s:ship) (r : spot array) = 
    Array.fold_left
      (fun c sp -> c + (if sp = HitShip s then 1 else 0))
      0 r in
  s.size = (Array.fold_left (fun c r -> c + dead_in_row s r) 0 g)


let did_lose b = List.fold_left
    (fun true_so_far s -> true_so_far && is_dead s b.grid) true b.ships


let shoot l b = 
  let x, y = row_col l in 
  match b.grid.(x).(y) with 
  | exception Invalid_argument(_)  -> raise InvalidLoc
  | Water -> b.grid.(x).(y) <- ShotWater;
    b.status <- Some "Your opponent missed.";
    "It's a miss!"
  | Ship s -> b.grid.(x).(y) <- HitShip s;
    let sh_name = string_of_ship s.name in
    if is_dead s b.grid then (
      (b.status <- Some ("Your opponent sank your "^sh_name^"."));
      ("It's a hit! You sunk your opponent's " ^ sh_name ^ "!")
    ) 
    else  (
      (b.status <- Some ("Your opponent shot your "^sh_name^"."));
      "It's a hit!"
    )
  | _ -> raise DuplicateShot


let setup_status b = 
  let on_board = List.filter (fun s -> s.on_board) b.ships in
  let off_board = List.filter (fun s -> not s.on_board) b.ships in
  "On the board: "
  ^ (long_string_of_ships on_board)
  ^ "\nOff the board: "
  ^ (long_string_of_ships off_board)

let status b = 
  let update = match b.status with 
    | None -> ""
    | Some m -> m in
  "You still have ships left. "^update

let complete b = 
  List.fold_left
    (fun true_so_far s -> true_so_far && s.on_board) true b.ships

(** [to_string_grid is_self b] is the grid (string list list) representation
    of board [b]. Represented as seen by the board's player if [is_self] is
    [true], and as seen by opponenets otherwise. *)
let to_string_grid is_self b =
  let rec row_str self g = function
    | [] -> []
    | Water::t -> (if self then "w" else "?")::(row_str self g t)
    | ShotWater::t -> (if self then "x" else "w")::(row_str self g t)
    | (Ship _)::t -> (if self then "O" else "?")::(row_str self g t)
    | (HitShip s)::t ->
      (if is_dead s g then "#" else "X")::(row_str self g t) in
  b.grid |> Array.to_list
  |> List.map (fun r -> row_str is_self b.grid (Array.to_list r))

let string_self (b:t) = to_string_grid true b

let string_other b = to_string_grid false b




