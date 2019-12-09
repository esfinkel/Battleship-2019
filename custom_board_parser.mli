(** Custom_board_parser allows the player to import various types of battleship
    games containing customized ship names, board sizes, and ship sizes
    using .json files. *)

(** Raised when the json is unable to be parsed. *)
exception ParsingError

(** Raised when a field is missing or of the wrong type. *)
exception MissingField

(** Raised when the board is illegal according to a number of arbitrary rules.
    String will contain helpful information. *)
exception InvalidBoardFile of string

(** Type representing the graphical mode of a game. *)
type graphicsMode = | SpaceMode | WaterMode

(** [get_board_from_file f] is [(board_size, mode, ships)] where [f] is the
    filepath of a valid json containing valid values corresponding to
    [board_size], [mode], and [ships].
    Raises:
    - [InvalidBoardFile s] if the board does not match certain parameters;
        [s] will contain helpful information (see [check_board]).
    - ParsingError if the json cannot be found or parsed.
    - MissingField if a required field is missing. *)
val get_board_from_file : string -> int * graphicsMode * (string*int) list