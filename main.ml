(** [print_help] prints the list of valid commands. *)
(*let print_help unit : unit = 
  ANSITerminal.(print_string [blue] 
                  (String.concat "" 
                     [ "\n\nGame set-up commands:";
                       "\nUse 'place' with the ship name and start location"
                       ^ "and end location on the board.";
                       "\nUse 'remove' and ship name to remove it from the board.";
                       "\nUse 'ready' when your board is set up and ready to play";
                       "\n\n Gameplay commands:";
                       "\nUse 'shoot' and a coordinate to shoot that spot";
                       "\nUse 'status' to see what ships you still have.";
                       "\nUse 'quit' to quit the game."
                     ])); ()

  let display_board board =


  let board_setup p1_board p2_board = 
    display_board p1_board;
    ANSITerminal.(print_string [blue]
                    "\n\nPlayer 1 please set up your board.\n");
    ()
*)
(** [main ()] prompts for the game to play, then starts it. *)
(*let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Battleship!\n");
  print_help ();
  board_setup (Board.init_board ()) (Board.init_board ())
*)
(* Execute the game engine. *)
(*let () = main () *)