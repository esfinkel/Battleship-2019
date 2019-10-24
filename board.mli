type t 

val init_board : unit -> t 

val place : Command.ship_name * Command.location * Command.location -> t -> unit 

val remove : Command.ship_name -> t -> unit 

val shoot : Command.location -> t -> unit

val status : t -> string 