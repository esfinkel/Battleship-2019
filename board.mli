(**Board initiate the battleship boards and keep track of ship
   locations, hit spots, and mine locations. *)
(** Raised when a player attempts to place a ship on a location that is
    off the board. *)
exception OffBoard
(** Raised when a player attempts to place a ship on coordinates that are 
    not in line with each other. (eg. a3 and b6) *)
exception Misaligned
(** Raised when a player attempts to place a ship on coordinates that are 
    the wrong distance away. (ie. The distance between the coordinates does
    not equal the size of the ship.) *)
exception WrongLength
(** Raised when a player attempts to place a ship on a location that is
    already occupied by another ship. *)
exception OverlappingShips 
(** Raised when a player attempts to place a ship that doesn't exist. *)
exception NoShip
(** Raised when a player attempts to shoot a spot that's already been shot. *)
exception DuplicateShot
(** Raised when a player attempts to place a ship that has an invalid name. *)
exception InvalidShipName

type t 


(** [init_board_default n] is a new [t], with all cells initialized to
    [Water] and player_name [n]. *)
val init_board_default : string -> t 

(** [init_board_from_file n f] is a new [t], with attributes from json with
    name [f], all cells initialized to [Water], and player_name [n]. *)
val init_board_from_file : string -> string -> t 

(** [min_ship_size b] is the length of the shortest ship of [b]. *)
val min_ship_size : t -> int

(** [board_size b] is the size of the square grid of board b. *)
val board_size : t -> int

(** [player_name b] is the name of the owner of [b]. *)
val player_name : t -> string

(** [place n l1 l2 b] places the ship with name [n] on [b], with its
    ends on locations represented by [l1] and [l2].
    If that ship was already on [b], it is removed before being re-placed.
    Raises:
    - OffBoard if [l1] or [l2] is off the game board
    - Misaligned if [l1] and [l2] are not in the same row or column
    - WrongLength if [l1] and [l2] are the wrong distance apart
    - OverlappingShips if the ship would overlap with a ship already
        present in [b]. *)
val place : string -> Command.location -> Command.location -> t -> unit 

(* i.e. place machine readable *)
(** [place_m_r n c1 c2 b] places the ship with name [n] on [b], with its
    ends are on locations represented by [c1] and [c2].
    If that ship was already on [b], it is removed before being re-placed.
    Raises:
    - OffBoard if [c1] or [c2] is off the game board
    - Misaligned if [c1] and [c2] are not in the same row or column
    - WrongLength if [c1] and [c2] are the wrong distance apart
    - OverlappingShips if the ship would overlap with a ship already
        present in [b]. *)
val place_m_r : string -> (int*int) -> (int*int) -> t -> unit

(** [did_lose b] is true iff all ships have been destroyed in [b]. *)
val did_lose : t -> bool

(** [shoot l b] is [(m, suc)]. [m] is a string message explaining the
    result of shooting location [l] on [b]. [suc] is true iff a ship
    has been shot.
    The location on [b] represented by [l] has now been shot; that
    location on [b] has been updated to reflect this information.
    Raises:
    - DuplicateShot if that location has already been shot.
    - InvalidLoc if that location is not on the board. *)
val shoot : Command.location -> t -> string * bool * bool 

(* i.e. shoot machine readable *)
(** [shoot_m_r coor b] is is (m, n). [m] is [true] iff a ship has been shot.
    [n] is true iff a ship has been shot and is now "dead".
    The location on [b] represented by [coor] has now been shot;
    that location on [b] has been updated to reflect this information.
    Raises:
    - DuplicateShot if that location has already been shot.
    - InvalidLoc if that location is not on the board.*)
val shoot_m_r : (int*int) -> t -> bool*bool

(** [setup_status b] is a string representing the status of [b], where
    [b] has not yet completed setup. *)
val setup_status : t -> string

(* i.e. setup status machine readable *)
(** [setup_status_m_r b] is a list of tuples [(n, s)] representing all of
    the ships that have not yet been placed, where each [n] is a ship's
    string name and the adjacent [s] is the size of the corresponding ship. *)
val setup_status_m_r : t -> (string * int) list

(** [status b] is a string representing the status of [b]. *)
val status : t -> string 

(** [complete b] is true iff [b] holds one of each of the ships that 
    belong to [b]. *)
val complete : t -> bool

(** [is_part_of_living_ship b (i, j)] is true iff there is a non-sunken ship
    on [b] at [(i, j)]. *)
val is_part_of_living_ship : t -> (int*int) -> bool 

(** [string_self b] is the grid representation of board [b], as seen by
    the board's player. *)
val string_self : t -> string list list

(** [string_other b] is the grid representation of board [b], as seen by
    other players. *)
val string_other : t -> string list list

(** [is_unshot b (i, j)] is true iff the cell on [b] at [(i, j)] has
    not been shot. *)
val is_unshot : t -> int*int -> bool

(** [place_mine b] places a mine on the board randomly if playing 
    mine game mode. *)
val place_mine : t -> int -> unit

(** [graphics_mode b] is the graphical mode of [b]. *)
val graphics_mode : t -> string