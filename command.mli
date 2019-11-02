(** The type [location] represents a specific location on the gameboard. 
    (e.g. ["A6"], ["B10"], etc.) *)
type location = string 

type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and and possibly an object phrase. *)
type command = 
  | Place of object_phrase
  | Shoot of object_phrase
  | Status
  | Help 
  | Quit 
  | Ready

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters.

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "place", "shoot", 
    "status", "help", "quit", nor "ready", or if the verb is "status", "help", 
    "ready" or "quit" and there is a non-empty object phrase, or if the verb 
    is "place", "remove", "shoot", or "status" and there is an empty object 
    phrase. *)
val parse : string -> command 


