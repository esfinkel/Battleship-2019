let clear_screen () =
  ANSITerminal.(erase Screen)


(** [print_grid grid] prints the string representation of grid
    (string list list) [grid]. *)
let print_grid grid =
  let print_cell c = ANSITerminal.( match c with
      | "w" -> print_string [blue] "w "
      | "x" -> print_string [white; on_black] "x "
      | "?" -> print_string [white; on_black] "? "
      | "O" -> print_string [white; on_black] "O "
      | "X" -> print_string [red] "X "
      | "#" -> print_string [red] "# "
      | _ -> ()
    )
  in
  let print_row i row =
    Char.chr (i+65) |> print_char;
    print_string " ";
    List.iter print_cell row;
    print_newline ()
  in
  let rec print_nums s e =
    if s <= e then (print_string " "; print_int s; print_nums (s+1) e) else ()
  in
  print_newline ();
  print_string " ";
  print_nums 1 (List.length (List.nth grid 0));
  print_newline ();
  List.iteri print_row grid

(** [print_self_board b] prints the colorful string representation of
    board [b], as seen by the board's player. *)
let print_self_board b =
  b |> Board.string_self |> print_grid

(** [print_other_board b] prints the colorful string representation of
    board [b], as seen by other playesr. *)
let print_other_board b =
  b |> Board.string_other |> print_grid



(** [print_help] prints the list of valid commands. *)
let print_help unit : unit = 
  ANSITerminal.(print_string [blue] 
                  (String.concat "" 
                     [ "\n\nGame set-up commands:";
                       "\nUse 'place' with the ship name and start location"
                       ^ "and end location on the board.";
                       "\nUse 'remove' and ship name to remove it from the board.";
                       "\nUse 'ready' when your board is set up and ready to play.";
                       "\n\n Gameplay commands:";
                       "\nUse 'shoot' and a coordinate to shoot that spot";
                       "\nUse 'status' to see what ships you still have.";
                       "\nUse 'quit' to quit the game."
                     ])); ()

let read_command unit : string =
  print_string "\n> ";
  match read_line () with
  | exception End_of_file -> "End of file exception thrown."
  | new_command -> new_command

let display_board board =
  ()

let try_placing (ship_phrase: string list) board =
  match ship_phrase with
  | name::l1::l2::[] ->
    (match Board.place name l1 l2 board with
     | exception Board.OffBoard -> 
       ANSITerminal.
         (print_string [red] ("\n\nYou cannot place the ship"
                              ^ "there.\nPlease enter coordinates that are on" 
                              ^ " the board."));
     | exception Board.Misaligned -> 
       ANSITerminal.
         (print_string [red]
            ("\n\nYou cannot place the ship with those "
             ^ "coordinates. Coordinates must be in the "
             ^ "same row or column."));
     | exception Board.WrongLength -> 
       ANSITerminal.
         (print_string [red]
            ("\n\nYou cannot place this ship with "
             ^ "those coordinates. The ship must have" 
             ^ " the right length"));
     | exception Board.DuplicateShip -> 
       ANSITerminal.
         (print_string [red]
            ("\n\nYou already placed that ship. Try placing a different one."));
     | exception Board.OverlappingShips ->
       ANSITerminal.(print_string [red] 
                       ("\n\nYou cannot place that ship there."
                        ^ " Another ship is already there."));
     | _ -> print_string ("\n\nYou placed the "  ^  name))
  | _ -> print_string "\n parsing error"


let try_removing ship_phrase board =
  ()


let rec continue_setup p1_board  = 
  match Command.parse (read_command ()) with
  | Place ship_phrase -> try_placing ship_phrase p1_board; 
    display_board p1_board; if Board.complete p1_board then () else
      continue_setup p1_board 
  | Remove ship_phrase -> try_removing ship_phrase p1_board;
    display_board p1_board;
    continue_setup p1_board 
  | Help -> print_help (); 
    continue_setup p1_board 
  | Quit -> ()
  | Ready -> ()
  | Status -> ANSITerminal.(print_string [red] "\n\nYou cannot check your game status until you begin playing.");
    continue_setup p1_board
  | Shoot _ -> ANSITerminal.(print_string [red] "\n\nYou cannot check your game status until you begin playing.");
    continue_setup p1_board
  | exception Command.Malformed -> ANSITerminal.(print_string [red] "Please input a valid command.");
    continue_setup p1_board 
  | exception Command.Empty -> ANSITerminal.(print_string [red] "Please input a valid command.");
    continue_setup p1_board 

(** [p1_setup p1_board] starts the process of setting up Player 1's board.*)
let p1_setup p1_board  =
  display_board p1_board;
  ANSITerminal.(print_string [blue]
                  ("\n\nPlayer 1 please set up your board." 
                   ^ "\nUse 'place' <ship name> 'on' <coordinate 1> <coordinate 2>"
                   ^ "\nUse 'remove' <ship name> to remove a ship."
                   ^ "\nUse 'ready' when all your ships are placed to continue."));
  continue_setup p1_board

(** [p2_setup p2_board] starts the process of setting up Player 2's board.*)
let p2_setup p2_board =
  display_board p2_board;
  ANSITerminal.(print_string [blue]
                  ("\n\nPlayer 2 please set up your board." 
                   ^ "\nUse 'place' <ship name> 'on' <coordinate 1> <coordinate 2>"
                   ^ "\nUse 'remove' <ship name> to remove a ship."
                   ^ "\nUse 'ready' when all your ships are placed to continue."));
  continue_setup p2_board

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Battleship!\n");
  print_help ();
  let p1_board = Board.init_board () in
  let p2_board = Board.init_board () in
  p1_setup p1_board;
  p2_setup p2_board;
  ()


(* Execute the game engine. *)
let () = main () 