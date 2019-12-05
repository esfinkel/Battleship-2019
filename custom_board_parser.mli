exception ParsingError
exception InvalidBoardFile of string

(** [get_board_from_file f] is [(board_size, mode, ships)] where [f] is the
    filepath of a valid json containing valid values corresponding to
    [board_size], [mode], and [ships].
    Raises:
    - [InvalidBoardFile s] if the board does not match certain parameters;
        [s] will contain helpful information (see [check_board]).
    - ParsingError if the json cannot be found or parsed. *)
val get_board_from_file : string -> int * string * (string*int) list