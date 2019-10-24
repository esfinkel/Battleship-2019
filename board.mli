(* move these to the ml file *)
(* AF : *)
(* RI : *)

exception OffBoard
exception Misaligned (* boat locations are not same row/column *)
exception WrongLength (* boat locations are wrong distance apart *)
exception OverlappingBoats 
exception NoBoat

(** The abstract type of values representing one player's battleship board. *)
type t 

type ship 

type spot
(* variant type opponent_spot (hidden | hit_ship | shot_missed) *)
(* variant type Spot (MySpot of my_spot | OpponentSpot of opponent_spot) *)

(** [init_board ()] is a new [t], with all cells initialized
    to [Water] *)
val init_board : unit -> t 

(** [place (n, l1, l2) b] is [()]. If legal, [b] now has a ship with 
    name [n], and its ends are in locations on t represented by
    [l1] and [l2].
    Throws:
    - OffBoard if [l1] or [l2] is off the game board
    - Misaligned if [l1] and [l2] are not in the same row or column
    - WrongLength if [l1] and [l2] are the wrong distance apart
    - OverlappingBoats if the boat would overlap with a boat already
        present in t. *)
val place : Command.ship_name * Command.location * Command.location -> t -> unit 

(** [remove n b] is [()]. If a ship with name [n] was present in [b], it
    has been removed, and the cells replaced with Water.
    Otherwise, raise NoBoat. *)
val remove : Command.ship_name -> t -> unit 

val shoot : Command.location -> t -> unit

val status : t -> string 