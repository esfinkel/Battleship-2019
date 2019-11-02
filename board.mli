
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

(** The abstract type of values representing one player's battleship board. *)
type t 

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

(** [did_lose b] is true if all ships have been destroyed in [b]. 
    False otherwise. *)
val did_lose : t -> bool

(** [complete b] is true iff [b] holds one of all of the ships
    in Command.ship_name. *)
val complete : t -> bool

(** [shoot l b] is [()]. The location on [b] reprented by [l] has now
    been shot.
    Raises:
    - DuplicateShot if that location has already been shot. *)
val shoot : Command.location -> t -> string

(** [setup_status b] is a string representing the status of [b], where
    [b] has not yet completed setup. *)
val setup_status : t -> string

(** [status b] is a string representing the status of [b]. *)
val status : t -> string 

(** [string_self b] is the grid (string list list) representation of
    board [b], as seen by the board's player. *)
val string_self : t -> string list list

(** [string_other b] is the grid (string list list) representation of
    board [b], as seen by other players. *)
val string_other : t -> string list list
