(** Raised when a player attempts to shoot or place a ship on 
    a location that isn't on the board. *)
exception InvalidLoc

(** [coor_type] is a type for values representing grid coordinates. *)
type coor_type = int * int

(** [row_col loc] is the [(row, column)] coordinate pair corresponding
    to [loc]. *)
let row_col (loc : Command.location) : (int*int) =
  let r = Str.regexp "\\([a-z]\\)\\([0-9]+\\)" in
  let pull_regex s =
    Str.global_replace r "\\1 \\2" (String.lowercase_ascii s)
    |> String.split_on_char ' ' in
  let index (c:char) : int = Char.code c - 97 in
  let tup = function
    | letter::number::[] -> (
        String.get letter 0 |> index,
        Stdlib.int_of_string number - 1
      )
    | _ -> raise InvalidLoc
  in
  loc |> pull_regex |> tup

(** [rev_row_col loc] takes in a pair of ints in the range 0 to 15 and 
    converts it to a string representing a location on the board. *)
let rev_row_col loc : Command.location = 
  let (i, j) = loc in 
  String.make 1 (Char.chr (65 + i)) ^ string_of_int (j + 1)

(** [get_letter n] is the nth letter of the alphabet, where ["A"] is #0. *)
let get_letter n = Char.chr (n+65)
                   |> String.make 1

(** [choose_random_letter bound] is a random letter represented as a string
    from the first [bound] letters of the alphabet.
    Precondition: 0 < bound <= 26. *)
let choose_random_letter bound = Random.int bound
                                 |> get_letter

(** [random_coors()] is a random coordinate string on the board. *)
let random_coor_string bound =
  let yaxis = choose_random_letter bound in
  let xaxis = string_of_int ((Random.int bound) + 1) in
  yaxis ^ xaxis


(** [ordered_coors c1 c2] is [(c1, c2)], except they are swapped
    if given in the reverse order.
    Precondition to comparability is that [l1] and [l2] are in either the
    same row or the same column. *)
let ordered_coors c1 c2 = if c1 < c2 then c1, c2 else c2, c1

(** [ordered l1 l2] is [(l1_i, l1_j), (l2_i, l2_j)], the coordinates of
    location [l1] and location [l2] respectively, except they are swapped
    if given in the reverse order.
    Precondition to comparability is that [l1] and [l2] are in either the
    same row or the same column. *)
let ordered_strings l1 l2 = let pos1, pos2 = (row_col l1), (row_col l2) in
  ordered_coors pos1 pos2


(** [from_file key] is the string in the string resource json with key
    [key]. *)
let from_file key =
  Yojson.Basic.(let j = from_file "string_res.json" in
                j |> Util.member key |> Util.to_string)

