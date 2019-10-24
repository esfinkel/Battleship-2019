(* move these to the ml file *)
(* AF : *)
(* RI : *)

(** The abstract type of values representing one player's battleship board. *)
type t 

type ship 

type spot
(* variant type opponent_spot (hidden | hit_ship | shot_missed) *)
(* variant type Spot (MySpot of my_spot | OpponentSpot of opponent_spot) *)

(** [init_board ()] is a new battleship board, with all cells initialized
    to Water *)
val init_board : unit -> t 

(** [place (n, l1, l2) s] is unit. *)
val place : Command.ship_name * Command.location * Command.location -> t -> unit 

val remove : Command.ship_name -> t -> unit 

val shoot : Command.location -> t -> unit

val status : t -> string 