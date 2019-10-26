
exception OffBoard
exception Misaligned
exception WrongLength
exception DuplicateShip
exception OverlappingShips 

exception NoShip
exception DuplicateShot

exception InvalidLoc

(** The abstract type of values representing one player's battleship board. *)
type t 

(** The abstract type of values representing a ship. *)
type ship 

(** The abstract type of values representing a board spot. *)
type spot

(** [init_board ()] is a new [t], with all cells initialized
    to [Water] *)
val init_board : unit -> t 

(** [place (n, l1, l2) b] is [()]. If legal, [b] now has a ship with 
    name [n], and its ends are in locations on t represented by
    [l1] and [l2].
    Raises:
    - OffBoard if [l1] or [l2] is off the game board
    - Misaligned if [l1] and [l2] are not in the same row or column
    - WrongLength if [l1] and [l2] are the wrong distance apart
    - DuplicateShip if the ship has already been placed
    - OverlappingShips if the ship would overlap with a ship already
        present in [b]. *)
val place : string -> Command.location -> Command.location -> t -> unit 

(** [remove n b] is [()]. If a ship with name [n] was present in [b], it
    has been removed, and the cells replaced with Water.
    Raises:
    - NoShip if that ship has not been placed. *)
val remove : Command.ship_name -> t -> unit 

(** [complete b] is true iff [b] holds one of all of the ships
    in Command.ship_name. *)
val complete : t -> bool

(** [shoot l b] is [()]. The location on [b] reprented by [l] has now
    been shot.
    Raises:
    - DuplicateShot if that location has already been shot. *)
val shoot : Command.location -> t -> unit

(** [status b] is a string representing the status of [b]. *)
val status : t -> string 