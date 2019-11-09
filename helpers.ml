let alphalst = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

(** [rev_row_col loc] takes in a pair of ints in the range 0 to 9 and 
    converts it to a string representing a location on the board. *)
let rev_row_col loc : Command.location = 
  let (x, y) = loc in 
  List.nth alphalst x ^ string_of_int (y + 1)