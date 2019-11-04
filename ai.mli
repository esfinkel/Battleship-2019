(** [t] is the abstract type representing a computerized Battleship player. *)
type t

val init : unit -> t

val place_all_ships : t -> unit

val shoot_ship : t -> string

val get_board : t -> Board.t