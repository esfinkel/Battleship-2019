(** Ai_smart initiates an AI player that uses advanced battleship game theory to
    shoot spots with the highest likelihood of containing a ship. Ai_smart then
    shoots around a hit spot to sink a ship.

    Difficulty: Hard

    (Documentation for exposed functions is in Ai.)
*)
include (module type of Ai)