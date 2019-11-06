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
(** Raised when a player attempts to shoot a location that isn't on 
    the board. *)
exception InvalidLoc
(** Raised when a player attempts to place a ship that has an invalid name. *)
exception InvalidShipName

(** The abstract type of values representing one player's battleship board. *)
type t 

(** [row_col loc] is the [(row, column)] coordinate pair corresponding
    to [loc]. *)
val row_col : Command.location -> int * int

(** [init_board n] is a new [t], with all cells initialized to [Water] and
    player_name [n]. *)
val init_board : string -> t 

(** [player_name b] is the name of the owner of [b]. *)
val player_name : t -> string

(** [place n l1 l2 b] is [()]. If legal, [b] now has a ship with 
    name [n], and its ends are in locations on t represented by
    [l1] and [l2].
    If that ship was already on [b], it is removed before being re-placed.
    Raises:
    - OffBoard if [l1] or [l2] is off the game board
    - Misaligned if [l1] and [l2] are not in the same row or column
    - WrongLength if [l1] and [l2] are the wrong distance apart
    - OverlappingShips if the ship would overlap with a ship already
        present in [b]. *)
val place : string -> Command.location -> Command.location -> t -> unit 

(** [did_lose b] is true iff all ships have been destroyed in [b]. *)
val did_lose : t -> bool

(** [shoot l b] is a string message explaining the result of the shot.
    The location on [b] reprented by [l] has now been shot;
    that location on [b] has been updated to reflect this information.
    Raises:
    - DuplicateShot if that location has already been shot. *)
val shoot : Command.location -> t -> string

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

(** [string_self b] is the grid (string list list) representation of
    board [b], as seen by the board's player. *)
val string_self : t -> string list list

(** [string_other b] is the grid (string list list) representation of
    board [b], as seen by other players. *)
val string_other : t -> string list list
