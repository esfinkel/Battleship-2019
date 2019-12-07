(** Shared module type for all AI modules. *)

(** [t] is the abstract type representing a computerized Battleship player. *)
type t

(** [init ()] is a new [t] representing a computerized Battleship player and
    the computer's default-design board. *)
val init : unit -> t

(** [init_custom f] is a new [t] representing a computerized Battleship
    player and the computer's board (with design based on the json at 
    filepath [f]). *)
val init_custom : string -> t

(** [place_all_ships ()] places all of the ships on the board in legal 
    positions. *)
val place_all_ships : t -> unit

(** [shoot_ship ai b] shoots a legal location on player's board [b], and 
    updates the board accordingly.  *)
val shoot_ship : t -> Board.t -> string

(** [get_board ai] is the computerized player's board. *)
val get_board : t -> Board.t