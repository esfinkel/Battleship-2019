(** [print_help] prints the list of valid commands. *)
let print_help unit : unit = 
  ANSITerminal.(print_string [blue] 
                  (String.concat "" 
                     [ "\n\nGame set-up commands:";
                       "\nUse 'place' with the ship name and start location"
                       ^ "and end location on the board.";
                       "\nUse 'remove' and ship name to remove it from the board.";
                       "\nUse 'ready' when your board is set up and ready to play";
                       "\n\n Gameplay commands:";
                       "\nUse 'shoot' and a coordinate to shoot that spot"
                     ])); ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Battleship!\n");
  print_help ();
  failwith "unimplemented"

(* Execute the game engine. *)
let () = main ()