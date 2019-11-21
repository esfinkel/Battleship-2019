(* For generating actual random numbers. *)
let () = Random.self_init ()

(** [single_difficulty] represents the type of ai to use for the computer. *)
type single_difficulty = 
  | Easy of Ai_random.t 
  | Medium of Ai_normal.t
  | Hard of Ai_smart.t

(** [clear_screen ()] clears the terminal window. *)
let clear_screen () =
  ANSITerminal.(erase Screen; erase Screen; erase Screen; erase Screen);
  Sys.command("clear") |> ignore

(** [print_grid grid] prints the string representation of grid
    (string list list) [grid]. *)
let print_grid grid =
  let print_cell c = ANSITerminal.( match c with
      | "w" -> print_string [cyan; on_blue]         "[ ]" (* " ■ " *)
      (* | "w" -> print_string [white; on_blue] "🌊 " *)
      | "x" -> print_string [white; on_blue]        "🌀 "
      | "?" -> print_string [on_blue]               "❔ "
      (* | "O" -> print_string [white; on_black]       " o " *)
      | "-" -> print_string [white; on_magenta; Bold] "🚢 " (* "═══" or === *)
      (* | "-" -> print_string [white; on_black; Bold] "🚢 " (* "═══" or === *) *)
      | "|" -> print_string [white; on_green; Bold] "🚢 " (* " ║ " *)
      (* | "|" -> print_string [white; on_black; Bold] "🚢 " " ║ "  *) 
      | "X|" | "X-" -> print_string [on_red]  "💥 "
      (* | "X" *)
      | "#" -> print_string [on_blue]               "🔥 "
      | "b" -> print_string [on_blue]               "💣 "
      | "B" -> print_string [on_blue]               "💥 "
      | _ -> ()
    )
  in
  let print_row i row =
    Char.chr (i+65) |> print_char;
    print_string " ";
    List.iter print_cell row;
    Char.chr (i+65) |> print_char;
    print_newline ()
  in
  let rec print_nums s e =
    if s <= e then ( print_int s;
                     if s>=9 then print_string " " else print_string "  ";
                     print_nums (s+1) e)
    else ()
  in
  print_newline ();
  print_string "   ";
  print_nums 1 (List.length (List.nth grid 0));
  print_newline ();
  List.iteri print_row grid;
  print_string "   ";
  print_nums 1 (List.length (List.nth grid 0));
  print_newline ()





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
  ANSITerminal.(
    print_string [cyan] 
      (String.concat "\n" 
         [ "\n\nGame set-up commands:";
           "Use 'place' <ship name> 'on' <coordinate 1> <coordinate 2>."
           ^ " to place a ship on the board.";
           "Use 'ready' when your board is set up and ready to play.";
           "\n Gameplay commands:";
           "Use 'shoot' <coordinate> to shoot that spot.";
           "Use 'status' to see what ships you still have.";
           "Use 'quit' to quit the game."
         ])); ()

let hit_sound () =
  try Sys.command "afplay audio/boom_midlength.m4a -v 0.4 & :" |> ignore
  with | _ -> ()

let splash_sound () =
  try Sys.command "afplay audio/splash_midlength.m4a -v 0.2 & :" |> ignore
  with | _ -> ()

let shoot_sound suc =
  let with_sound = Sys.argv.(1) in
  if with_sound = "1" then
    if suc then hit_sound () else splash_sound ()
  else ()

(** [read_command] returns a user string input from the command line. *)
let read_command unit : string =
  print_string "\n> ";
  match String.lowercase_ascii (String.trim (read_line ())) with
  | "quit" -> exit 0
  | exception End_of_file -> "End of file exception thrown."
  | new_command -> new_command

(** [display_board ob b] displays a hidden version of the opponents board [ob] 
    and a visible version of the players board [b]. *)
let display_board other_board my_board =
  print_other_board other_board;
  print_self_board my_board

(** [try_placing ship_phrase board] attempts to place a ship on the board. *)
let try_placing (ship_phrase: string list) board =
  match ship_phrase with
  | name::l1::l2::[] -> (
      match Board.place name l1 l2 board with
      | exception Board.OffBoard -> 
        ANSITerminal.
          (print_string [red] (
              "\n\nYou cannot place the ship"
              ^ "there.\nPlease enter coordinates that are on" 
              ^ " the board.")
          );
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
              ^ " the right length."));
      | exception Board.InvalidShipName ->
        ANSITerminal.(print_string [red] 
                        ("\n\nYou cannot place that ship."
                         ^ " Please enter a valid ship name."));
      | exception Board.OverlappingShips ->
        ANSITerminal.(print_string [red] 
                        ("\n\nYou cannot place that ship there."
                         ^ " There is already a ship on those coordinates."
                         ^ " Try placing the ship on a different location."));
      | () -> print_self_board board;
        print_endline ("\n\nYou placed the "  ^  name ^ ".");
        Board.setup_status board |> print_endline
    )
  | _ -> print_endline "\n parsing error"

(** [continue_setup board] reads in a command, parses it, and executes it. *)
let rec continue_setup board  = 
  match Command.parse (read_command ()) with
  | Place ship_phrase -> try_placing ship_phrase board; 
    if Board.complete board then
      print_endline "\nAll ships placed.\nType 'ready' to continue." else ();
    continue_setup board 
  | Help -> print_help (); 
    continue_setup board 
  | Ready -> if Board.complete board then () else
      (ANSITerminal.(print_string [red]
                       "No you're not! Make sure all your ships are placed.");
       continue_setup board)
  | Status -> ANSITerminal.(
      print_string [red]
        "\n\nYou cannot check your game status until you begin playing."
    );
    continue_setup board
  | Shoot _ -> ANSITerminal.(
      print_string [red]
        "\n\nYou cannot shoot until you begin playing."
    );
    continue_setup board
  | exception Command.Malformed -> ANSITerminal.(
      print_string [red] "Please input a valid command."
    );
    continue_setup board 
  | exception Command.Empty -> ANSITerminal.(
      print_string [red] "Please input a valid command."
    );
    continue_setup board 

(**  [wait_next_move ()] clears the terminal screen and waits
     for the next player to press enter to start their turn. *)
let wait_next_move () = 
  clear_screen ();
  ANSITerminal.(
    print_string [cyan] "Press enter to reveal your board.");
  match read_command () with 
  | _ -> ()

(** [pause ()] waits for the current player to press enter
    after their turn. *)
let pause () =
  ANSITerminal.(
    print_string [cyan] "Please press enter, then switch players!");
  match read_command () with 
  | _ -> wait_next_move ()

(** [setup board] starts the process of setting up [board]. *)
let setup board  =
  print_self_board board;  Board.setup_status board |> print_endline;
  ANSITerminal.(
    print_string [cyan]
      ("\n\n"^(Board.player_name board)^": Please set up your board." 
       ^ "\nUse 'place' <ship name> 'on' <coordinate 1> <coordinate 2>."
       ^ "\nUse 'ready' when all your ships are placed to continue.")
  );
  continue_setup board

let win_message = ": You won the game! Congratulations! \n\n"
let lose_message = ": You lost! Better luck next time! \n\n"

(** [display_two_player_end_message winner_board loser_board] displays that 
    [winner_board.player_name] won the game and [loser_board.player_name]
    lost. *)
let display_two_player_end_message loser_board winner_board = 
  print_string "\n\n\n\n";
  print_self_board loser_board;
  ANSITerminal.(
    print_string [yellow]
      ("Player "
       ^(Board.player_name loser_board)
       ^lose_message));
  print_self_board winner_board;
  ANSITerminal.(
    print_string [yellow]
      ("Player "
       ^(Board.player_name winner_board)
       ^win_message))

(** [display_win_message winner_board] displays that [loser_board.player_name] 
    won the game iff [won], or that they lost otherwise. *)
let display_one_player_end_message won winner_board loser_board = 
  print_string "\n\n\n\n";
  print_self_board winner_board;
  print_self_board loser_board;
  ANSITerminal.(
    print_string [yellow]
      ("Player "
       ^(Board.player_name loser_board)
       ^(if won then win_message else lose_message)))

(** [try_shooting shoot_phrase target_board my_board] is [true] if the game 
    should continue and [false] if there is a parsing error.  It attempts to 
    shoot the spot stated in [shoot_phrase] on [target_board] and if a player 
    has won, it displays the appropriate winning/losing message and exits 
    the game. *)
let rec try_shooting shoot_phrase target_board my_board =
  match shoot_phrase with 
  | loc::[] -> begin 
      match Board.shoot loc target_board with 
      | exception Board.DuplicateShot -> ANSITerminal.(
          print_string [red] ("You've already shot there! Try shooting " 
                              ^ "somewhere else!")
        ); false
      | exception Board.InvalidLoc -> ANSITerminal.(
          print_string [red] "That's not on the board!"
        ); false
      | message, success -> display_board target_board my_board; 
        shoot_sound success;
        ANSITerminal.( 
          print_string [cyan] ("You shot: " ^ loc ^ ".\n");
          print_string [cyan] message; print_newline (););
        if (Board.did_lose target_board) then 
          (display_two_player_end_message target_board my_board;
           exit 0 )
        else 
          pause ();
        true
    end
  | _ -> print_endline "\n parsing error"; false

(** [continue_game board o_board] reads in a command, parses it, and 
    executes it. *)
let rec continue_game board o_board = 
  match Command.parse (read_command ()) with
  | Place _ -> ANSITerminal.( 
      print_string[red] "You can't move your ships during the game!");
    continue_game board o_board
  | Help -> print_help (); 
    continue_game board o_board
  | Ready -> ANSITerminal.(
      print_string [red] "Please input a valid command."
    );
    continue_game board o_board
  | Status -> ANSITerminal.(
      print_string [cyan] (Board.status board)
    );
    continue_game board o_board
  | Shoot shoot_phrase -> if try_shooting shoot_phrase o_board board then () 
    else continue_game board o_board
  | exception Command.Malformed -> ANSITerminal.(
      print_string [red] "Please input a valid command."
    );
    continue_game board o_board
  | exception Command.Empty -> ANSITerminal.(
      print_string [red] "Please input a valid command."
    );
    continue_game board o_board

(** [next_move board o_board] prompts the player for a gameplay
    command, which it then processes. *)
let rec next_move board o_board = 
  clear_screen ();
  display_board o_board board; 
  ANSITerminal.(
    print_string [cyan]
      ("\n\n"
       ^ (Board.status board) ^ "\n"
       ^(Board.player_name board)
       ^": Please make your move." 
       ^ "\nUse 'shoot' <coordinate 1> to shoot that location."
       ^ "\nUse 'status' to check your status."));
  continue_game board o_board;
  next_move o_board board (* boards are swapped! *)

(** [check_p2_name p1_name] is [s], where [s] is a new player's name.
    The function recurses if [s] is blank, or the same as [p1_name]. *)
let rec check_p2_name p1_name =
  let x = read_command () in 
  if (x = p1_name) || (
      x 
      |> String.split_on_char ' ' 
      |> List.filter (fun x -> x <> "") = []
    )  
  then (print_endline "Please enter a valid name."; 
        check_p2_name p1_name) else x

(** [get_names ()] is [(n1, n2)]; it asks each player for their names,
    and [n1] and [n2] are those names. *)
let get_names () =  print_string "Player 1 name?";
  let p1_name = check_p2_name "" in
  print_string "Player 2 name?";
  let p2_name = check_p2_name p1_name in 
  (p1_name, p2_name)

(** [choose_mines] prompts the player to enter a number of mines to place on
    each board. Both boards should have an equal number of bombs. *)
let rec choose_mines () =
  print_string "\nHow many mines would you like to put in the boards: 0 to 10";
  try
    match read_command () with
    | num -> let n = int_of_string num in 
      if (int_of_string num ) >=0 && (int_of_string num) <= 10 then n else begin
        ANSITerminal.(print_string [red]
                        "\n\nEnter a number of mines from 0 to 10"); 
        choose_mines () end
  with _ -> ANSITerminal.(print_string [red]
                            "\n\nEnter a number of mines from 0 to 10"); 
    choose_mines ()

(** [multiplayer ()] prompts for the multiplayer game to play, then
    starts it.*)
let multiplayer () =
  let p1, p2 = get_names () in
  let p1_board = Board.init_board p1 in
  let p2_board = Board.init_board p2 in
  let mine_count = choose_mines () in
  clear_screen ();
  setup p1_board; Board.place_mine p1_board mine_count; clear_screen ();
  setup p2_board; Board.place_mine p2_board mine_count; clear_screen ();
  ANSITerminal.(print_string [cyan]
                  ("Player "^(Board.player_name p1_board)^
                   ": Please take control, then press enter!\n"));
  (match read_command () with | _ -> ());
  next_move p1_board p2_board

let rec get_name () : string = print_string "Player name?";
  let name = read_command () in
  if name = "" then (ANSITerminal.(print_string [red] 
                                     "\n\nEnter a valid name.\n");
                     get_name ())
  else name

(** [single_try_shooting shoot_phrase ai_board my_board] is [true] if the
    game should continue and [false] if there is a parsing error. It
    attempts to shoot the spot stated in [shoot_phrase] on [ai_board] and
    if the player has won or lost, it displays the appropriate winning/losing
    message and exits the game. *)
let rec single_try_shooting shoot_phrase ai_board my_board =
  match shoot_phrase with 
  | loc::[] -> begin 
      match Board.shoot loc ai_board with 
      | exception Board.DuplicateShot -> ANSITerminal.(
          print_string [red] ("You've already shot there! Try shooting " 
                              ^ "somewhere else!")
        ); false
      | exception Board.InvalidLoc -> ANSITerminal.(
          print_string [red] "That's not on the board!"
        ); false
      | message, success -> clear_screen (); shoot_sound success;
        ANSITerminal.( 
          print_string [cyan] ("You shot: " ^ loc ^ ".\n");
          print_string [cyan] message; print_newline (););
        if (Board.did_lose ai_board) then 
          (display_one_player_end_message true ai_board my_board;
           exit 0 )
        else 
          true
    end
  | _ -> print_endline "\n parsing error"; false

(** [single_continue_game player_board ai_board] reads in a command, parses
    it, and executes it. (Same as [next_move] except the other player is
    the AI.) *)
let rec single_continue_game player_board ai_board =
  match Command.parse (read_command ()) with
  | Place _ -> ANSITerminal.( 
      print_string[red] "You can't move your ships during the game!");
    single_continue_game player_board ai_board
  | Help -> print_help (); 
    single_continue_game player_board ai_board
  | Ready -> ()
  | Status -> ANSITerminal.(
      print_string [cyan] (Board.status player_board)
    );
    single_continue_game player_board ai_board
  | Shoot shoot_phrase -> 
    if single_try_shooting shoot_phrase ai_board player_board then () 
    else single_continue_game player_board ai_board
  | exception Command.Malformed -> ANSITerminal.(
      print_string [red] "Please input a valid command."
    );
    single_continue_game player_board ai_board
  | exception Command.Empty -> ANSITerminal.(
      print_string [red] "Please input a valid command."
    );
    single_continue_game player_board ai_board

(** [ai_shoot player_board single_dif] shoots the [player_board] based
    on the level of [single_dif]. *)
let ai_shoot player_board ai_player single_dif=
  match single_dif with
  | Easy ai_board -> ignore (Ai_random.shoot_ship ai_board player_board);
  | Medium ai_board -> ignore (Ai_normal.shoot_ship ai_board player_board);
  | Hard ai_board-> ignore (Ai_smart.shoot_ship ai_board player_board);
    if (Board.did_lose player_board) then 
      (display_one_player_end_message false ai_player player_board;
       exit 0 )
    else ()

(** [single_next_move player_board ai_board single_dif] prompts the player 
    for a gameplay command which it then processes in [single_continue_game].
    (Same as [next_move] except the other player is the AI.) *)
let rec single_next_move player_board ai_board single_dif =
  display_board ai_board player_board; 
  ANSITerminal.(
    print_string [cyan]
      ("\n\n"
       ^ (Board.status player_board) ^ "\n"
       ^(Board.player_name player_board)
       ^": Please make your move." 
       ^ "\nUse 'shoot' <coordinate 1> to shoot that location."
       ^ "\nUse 'status' to check your status."));
  single_continue_game player_board ai_board;
  ai_shoot player_board ai_board single_dif;
  single_next_move player_board ai_board single_dif (* boards are swapped! *)

(** [choose_difficulty ()] is the level of ai difficulty that the player
    chooses upon prompting. *)
let rec choose_difficulty () =
  print_string "\nChoose the game difficulty: easy, medium, or hard.";
  match read_command () with
  | "easy" -> Easy (Ai_random.init())
  | "medium" -> Medium (Ai_normal.init())
  | "hard" -> Hard (Ai_smart.init())
  | _ -> ANSITerminal.(print_string [red] 
                         "\n\nEnter 'easy', 'medium', or 'hard'."); 
    choose_difficulty ()

(** [singleplayer ()] prompts for the singleplayer game to play,
    then starts it.*)
let singleplayer () =
  let player = get_name () in
  let player_board = Board.init_board player in
  let single_dif = choose_difficulty () in
  let mine_count = choose_mines () in
  match single_dif with
  | Easy ai_player -> 
    Ai_random.place_all_ships ai_player;
    setup player_board; Board.place_mine player_board mine_count;
    Board.place_mine (Ai_random.get_board ai_player) mine_count;
    clear_screen ();
    single_next_move player_board (Ai_random.get_board ai_player) single_dif
  | Medium ai_player-> 
    Ai_normal.place_all_ships ai_player;
    setup player_board; Board.place_mine player_board mine_count; 
    Board.place_mine (Ai_normal.get_board ai_player) mine_count;
    clear_screen ();
    single_next_move player_board (Ai_normal.get_board ai_player) single_dif
  | Hard ai_player ->
    Ai_smart.place_all_ships ai_player;
    setup player_board; Board.place_mine player_board mine_count;
    Board.place_mine (Ai_smart.get_board ai_player) mine_count;
    clear_screen ();
    single_next_move player_board (Ai_smart.get_board ai_player) single_dif

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () = 
  print_string "\n1 player or 2 players? Enter '1' or '2'.";
  match read_command () with
  | "1" -> singleplayer ()
  | "1 player" -> singleplayer ()
  | "2" -> multiplayer ()
  | "2 players" -> multiplayer ()
  | _ -> ANSITerminal.(print_string [red] 
                         "\n\nEnter the number of players: 1 or 2.");
    main ()

(* Execute the game engine. *)
let () = clear_screen ();
  if Sys.argv.(1)="1" then Loading_screen.scroll_battleship () else ();
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to Battleship!\n");
  main () 