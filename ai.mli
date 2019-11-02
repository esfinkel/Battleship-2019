(** [t] is the abstract type representing a computerized Battleship player. *)
type t

val init : unit -> t

val place_all_ships : unit -> unit

val shoot_ship : unit -> unit
