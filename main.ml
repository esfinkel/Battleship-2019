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
let print_grid mode grid =
  let spacemode = mode = "space" in
  let print_cell c = ANSITerminal.(
      let ship = if spacemode then "🚀 " else "🚢 " in
      match c with
      | "w" ->
        let colors =
          if spacemode then [white; on_black] else [cyan; on_blue] in
        print_string colors         "[ ]" (* " ■ " *)
      | "x" -> if spacemode then print_string [on_black]  "🕳️  "
        else print_string [on_blue]        "🌀 "
      | "?" -> print_string [if spacemode then on_black else on_blue] "❔ "
      | "-" -> print_string [on_magenta] ship (* "═══" or === *)
      | "|" -> print_string [on_green] ship (* " ║ " *)
      | "X|" | "X-" -> print_string [on_red]  "💥 "
      | "#" -> print_string [on_blue]               "🔥 "
      | "b" -> print_string [if spacemode then on_black else on_blue] "💣 "
      | "B" -> print_string [on_blue]               "💥 "
      | _ -> ()
    )
  in
  let print_row i row =
    Helpers.get_letter i |> print_string;
    print_string " ";
    List.iter print_cell row;
    Helpers.get_letter i |> print_string;
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
let print_self_board mode b =
  b |> Board.string_self |> print_grid mode

(** [print_other_board b] prints the colorful string representation of
    board [b], as seen by other playesr. *)
let print_other_board mode b =
  b |> Board.string_other |> print_grid mode

(** [print_help] prints the list of valid commands. *)
let print_help unit : unit = 
  ANSITerminal.(print_string [cyan] (Helpers.from_file "main_help"))

(** [hit_sound ()] plays the hit-ship sound. *)
let hit_sound () =
  try Sys.command "afplay audio/boom_midlength.m4a -v 1.0 & :" |> ignore
  with | _ -> ()

(** [splash_sound ()] plays the splash sound. *)
let splash_sound () =
  try Sys.command "afplay audio/splash_midlength.m4a -v 0.3 & :" |> ignore
  with | _ -> ()

(** [bomb_sound ()] plays the bomb sound. *)
let bomb_sound () = 
  try Sys.command "afplay audio/bombexplosion.mp3 -v 0.5 & :" |> ignore
  with | _ -> ()

(** [shoot_sound suc bomb_suc] plays the appropriate sound. *)
let shoot_sound suc bomb_suc =
  let with_sound = Sys.argv.(1) in
  if with_sound = "1" then
    if bomb_suc then bomb_sound () 
    else if suc then hit_sound ()
    else splash_sound ()
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
  let mode = Board.graphics_mode other_board in
  print_other_board mode other_board;
  print_self_board mode my_board

(** [try_placing ship_phrase board] attempts to place a ship on the board. *)
let try_placing (ship_phrase: string list) board =
  match ship_phrase with
  | name::l1::l2::[] -> (
      match Board.place name l1 l2 board with 
      | exception Board.OffBoard -> 
        ANSITerminal.(print_string [red] (Helpers.from_file "main_offboard"))
      | exception Helpers.InvalidLoc -> 
        ANSITerminal.(print_string [red] (Helpers.from_file "main_offboard"))
      | exception Board.Misaligned -> 
        ANSITerminal.
          (print_string [red] (Helpers.from_file "main_misaligned"))
      | exception Board.WrongLength -> 
        ANSITerminal.
          (print_string [red] (Helpers.from_file "main_wronglength"));
      | exception Board.InvalidShipName ->
        ANSITerminal.(print_string [red] 
                        (Helpers.from_file "main_invalidname"));
      | exception Board.OverlappingShips ->
        ANSITerminal.(print_string [red] 
                        (Helpers.from_file "main_overlapping"));
      | () -> print_self_board (Board.graphics_mode board) board;
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
                       (Helpers.from_file "main_ready_setup_error"));
       continue_setup board)
  | Status -> ANSITerminal.(
      print_string [red]
        (Helpers.from_file "main_status_setup_error")
    );
    continue_setup board
  | Shoot _ -> ANSITerminal.(
      print_string [red]
        (Helpers.from_file "main_shoot_setup_error")
    );
    continue_setup board
  | exception Command.Malformed -> ANSITerminal.(
      print_string [red] (Helpers.from_file "main_invalid_command")
    );
    continue_setup board 
  | exception Command.Empty -> ANSITerminal.(
      print_string [red] (Helpers.from_file "main_invalid_command")
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
  print_self_board (Board.graphics_mode board) board; 
  Board.setup_status board |> print_endline;
  ANSITerminal.(
    print_string [cyan]
      ("\n\n"^(Board.player_name board)^Helpers.from_file "main_setup")
  );
  continue_setup board

(**[win_message] returns the win message found in [string_res.json] *)
let win_message = Helpers.from_file "main_win_message"
(**[lose_message] returns the lose message found in [string_res.json] *)
let lose_message = Helpers.from_file "main_lose_message"

(** [display_two_player_end_message winner_board loser_board] displays that 
    [winner_board.player_name] won the game and [loser_board.player_name]
    lost. *)
let display_two_player_end_message loser_board winner_board = 
  print_string "\n\n\n\n";
  let mode = (Board.graphics_mode loser_board) in
  print_self_board mode loser_board;
  ANSITerminal.(
    print_string [yellow]
      ("Player "
       ^(Board.player_name loser_board)
       ^lose_message));
  print_self_board mode winner_board;
  ANSITerminal.(
    print_string [yellow]
      ("Player "
       ^(Board.player_name winner_board)
       ^win_message))

(** [display_win_message winner_board] displays that [loser_board.player_name] 
    won the game iff [won], or that they lost otherwise. *)
let display_one_player_end_message won winner_board loser_board = 
  print_string "\n\n\n\n";
  let mode = (Board.graphics_mode loser_board) in
  print_self_board mode winner_board;
  print_self_board mode loser_board;
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
          print_string [red] ((Helpers.from_file "main_dup_shot"))
        ); false
      | exception Helpers.InvalidLoc -> ANSITerminal.(
          print_string [red] (Helpers.from_file "main_invalid_loc")
        ); false
      | message, success, bomb_suc -> display_board target_board my_board; 
        shoot_sound success bomb_suc;
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
      print_string[red] (Helpers.from_file "main_place_game_error"));
    continue_game board o_board
  | Help -> print_help (); 
    continue_game board o_board
  | Ready -> ANSITerminal.(
      print_string [red] (Helpers.from_file "main_invalid_command")
    );
    continue_game board o_board
  | Status -> ANSITerminal.(
      print_string [cyan] (Board.status board)
    );
    continue_game board o_board
  | Shoot shoot_phrase -> if try_shooting shoot_phrase o_board board then () 
    else continue_game board o_board
  | exception Command.Malformed -> ANSITerminal.(
      print_string [red] (Helpers.from_file "main_invalid_command")
    );
    continue_game board o_board
  | exception Command.Empty -> ANSITerminal.(
      print_string [red] (Helpers.from_file "main_invalid_command")
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
       ^(Helpers.from_file "main_move")));
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
  print_string (Helpers.from_file "main_mine_number");
  try
    match read_command () with
    | num -> let n = int_of_string num in 
      if (int_of_string num ) >=0 && (int_of_string num) <= 10 then n else
        begin
          ANSITerminal.(print_string [red]
                          "\n\nEnter a number of mines from 0 to 10"); 
          choose_mines ()
        end
  with _ -> ANSITerminal.(print_string [red]
                            "\n\nEnter a number of mines from 0 to 10"); 
    choose_mines ()

(** [multiplayer style] prompts for the multiplayer game to play,
    with board style [style], then starts it.*)
let multiplayer style =
  let p1, p2 = get_names () in
  let p1_board, p2_board = match style with
    | "default" -> Board.init_board_default p1, Board.init_board_default p2
    | f -> Board.init_board_from_file p1 f, Board.init_board_from_file p2 f
  in
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
          print_string [red] (Helpers.from_file "main_dup_shot")
        ); false
      | exception Helpers.InvalidLoc -> ANSITerminal.(
          print_string [red] (Helpers.from_file "main_invalid_loc")
        ); false
      | message, success, bomb_suc -> clear_screen (); 
        shoot_sound success bomb_suc;
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
      print_string[red] (Helpers.from_file "main_place_game_error"));
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
      print_string [red] (Helpers.from_file "main_invalid_command")
    );
    single_continue_game player_board ai_board
  | exception Command.Empty -> ANSITerminal.(
      print_string [red] (Helpers.from_file "main_invalid_command")
    );
    single_continue_game player_board ai_board

(** [ai_shoot player_board single_dif] shoots the [player_board] based
    on the level of [single_dif]. *)
let ai_shoot player_board single_dif=
  (match single_dif with
   | Easy ai_board -> Ai_random.shoot_ship ai_board player_board
   | Medium ai_board -> Ai_normal.shoot_ship ai_board player_board
   | Hard ai_board-> Ai_smart.shoot_ship ai_board player_board
  ) |> ignore;
  if Board.did_lose player_board
  then let ai_board = match single_dif with
      | Easy ai_p -> Ai_random.get_board ai_p
      | Medium ai_p -> Ai_normal.get_board ai_p
      | Hard ai_p -> Ai_smart.get_board ai_p in
    display_one_player_end_message false (ai_board) player_board;
    exit 0 
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
       ^(Helpers.from_file "main_move")));
  single_continue_game player_board ai_board;
  ai_shoot player_board single_dif;
  single_next_move player_board ai_board single_dif (* boards are swapped! *)

(** [make_ai_player style_dif] makes an ai player, in style [style], at the
    level of ai difficulty that the player chooses upon prompting. *)
let rec make_ai_player_dif style =
  print_string "\nChoose the game difficulty: easy, medium, or hard.";
  match style, read_command () with
  | "default", "easy" -> Easy (Ai_random.init())
  | f, "easy" -> Easy (Ai_random.init_custom f)
  | "default", "medium" -> Medium (Ai_normal.init())
  | f, "medium" -> Medium (Ai_normal.init_custom f)
  | "default", "hard" -> Hard (Ai_smart.init())
  | f, "hard" -> Hard (Ai_smart.init_custom f)
  | _ -> ANSITerminal.(print_string [red] 
                         "\n\nEnter 'easy', 'medium', or 'hard'."); 
    make_ai_player_dif style

(** [singleplayer style] prompts for the singleplayer game to play,
    with board style [style], then starts it.*)
let singleplayer style =
  let player = get_name () in
  let player_board = match style with
    | "default" -> Board.init_board_default player
    | f -> Board.init_board_from_file player f
  in
  let ai_player_with_diff = make_ai_player_dif style in
  let mine_count = choose_mines () in
  setup player_board; Board.place_mine player_board mine_count;
  match ai_player_with_diff with
  | Easy ai_player -> 
    Ai_random.place_all_ships ai_player;
    Board.place_mine (Ai_random.get_board ai_player) mine_count;
    clear_screen ();
    single_next_move player_board (Ai_random.get_board ai_player)
      ai_player_with_diff
  | Medium ai_player-> 
    Ai_normal.place_all_ships ai_player;
    Board.place_mine (Ai_normal.get_board ai_player) mine_count;
    clear_screen ();
    single_next_move player_board (Ai_normal.get_board ai_player)
      ai_player_with_diff
  | Hard ai_player ->
    Ai_smart.place_all_ships ai_player;
    Board.place_mine (Ai_smart.get_board ai_player) mine_count;
    clear_screen ();
    single_next_move player_board (Ai_smart.get_board ai_player)
      ai_player_with_diff

(**[board_style] prompts the player for a style of board. Can either be 
   [default] or a custom board found in a .json file. *)
let rec board_style () =
  print_string "\n'default' board? If not, enter filepath:";
  match read_command () with
  | "default" -> "default"
  | filepath ->
    (if Sys.file_exists filepath
     then 
       (try Custom_board_parser.get_board_from_file filepath |> ignore;
          filepath with
       | Custom_board_parser.ParsingError ->
         print_endline "Error parsing json."; board_style ()
       | Custom_board_parser.MissingField ->
         print_endline "json is missing a field."; board_style ()
       | Custom_board_parser.InvalidBoardFile s ->
         print_endline ("Your board file is invalid: "^s); board_style ()
       | _ -> print_endline "Unknown exception. Using default board.";
         "default"
       )
     else (print_endline "Invalid filepath."; board_style ())
    )

(** [main ()] prompts for the game to play, then starts it. *)
let rec main () = 
  let style = board_style () in
  print_string "\n1 player or 2 players? Enter '1' or '2'.";
  match read_command () with
  | "1" | "1 player" -> singleplayer style
  | "2" | "2 players" -> multiplayer style
  | _ -> ANSITerminal.(print_string [red] 
                         "\n\nEnter the number of players: 1 or 2."); main ()

(* Execute the game engine. *)
let () = clear_screen ();
  if Sys.argv.(1)="1" then Loading_screen.scroll_battleship () else ();
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to Battleship!\n");
  main () 