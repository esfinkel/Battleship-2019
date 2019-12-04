


let alphalst = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o"]

(** [rev_row_col loc] takes in a pair of ints in the range 0 to 9 and 
    converts it to a string representing a location on the board. *)
let rev_row_col loc : Command.location = 
  let (i, j) = loc in 
  List.nth alphalst i ^ string_of_int (j + 1)



let from_file key =
  Yojson.Basic.(let j = from_file "string_res.json" in
                j |> Util.member key |> Util.to_string)

