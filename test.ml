(* Overall, our test suite encompasses all things that could reasonably be 
   tested without gameplay. For everything else, (such as AI shooting in single
   player mode) we tested the functionality through playing our game, and by
   making sure everything proceeded as expected while we played. To do this, we 
   tested that all of the error messages for invalid inputs worked as we hoped 
   and that everything involving game setup, two-player and one-player modes, 
   and all expected functionality with our game worked as planned. For more 
   details on what we tested in each file, how we tested it, and why we may 
   or may not have left it out of the test suite, we explain all of the relevant
   files below:

   We tested all of the functions in command.mli, board.mli, ai_normal.mli, 
   ai_smart.mli, and ai_random.mli. 

   For command.ml, our bisect gave us a code coverage of 100 percent. This is 
   because command.ml only has to do with parsing which it is very easy to 
   test all of the outcomes and situations for. Also, we've played the game
   through fully enough while paying attention/testing parsing details to 
   tell that everything is working properly. 

   For board.ml, our bisect gave us a code coverage of 76.70 percent.

   For ai_random.ml, our bisect gave us a code coverage of 91.67 percent. The
   only parts of the easiest/random AI we couldn't put into the test file 
   were the cases where there was a successful shot. Since the AI fires shots 
   randomly, we found it better to test this function with gameplay to make 
   sure it always functioned properly and as expected. 

   For ai_normal.ml, our bisect gave us a relatively low code coverage. The main
   reason for this is

   For ai_smart.ml, our bisect once again gave us a relatively low code 
   coverage. The main reason for this is *)

open OUnit2
open Command
open Board
open Ai
open Helpers
open Custom_board_parser

(** [make_parse_test name str_input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with 
    [parse str_input]. *)
let make_parse_test 
    (name : string)
    (str_input : string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str_input))

(** [make_exn_raised_test name func input expected_exn] constructs an OUnit
    test  named [name] that asserts [func input] raises the
    [expected_exn]. *)
let make_exn_raised_test
    (name : string)
    func
    input
    (expected_exn : exn) : test = 
  name >:: (fun _ -> 
      assert_raises expected_exn (fun () -> func input))

let make_parse_exn_test name = make_exn_raised_test name Command.parse 

let command_tests = [
  make_parse_test "normal place" "place ship on shot ship" 
    (Place ["ship"; "shot"; "ship"]);
  make_parse_test "place default" "place    default     " 
    (Place ["default";"";""]);
  make_parse_test "place random" "     place    random     " 
    (Place ["random";"";""]);
  make_parse_test "place without on" "place    cruiser c1 c2     " 
    (Place ["cruiser";"c1";"c2"]);

  make_parse_test "normal shoot" "shoot Ship" 
    (Shoot ["Ship"]);
  make_parse_test "shoot with numbers and spaces" " shoot 6  " 
    (Shoot ["6"]);
  make_parse_test "shoot with s" " s a6  " 
    (Shoot ["a6"]);

  make_parse_test "normal status" "status" Status;
  make_parse_test "status with spaces" "status   " Status;

  make_parse_test "normal help" "help" Help;
  make_parse_test "help with spaces" "    help" Help;

  make_parse_test "normal ready" "ready" Ready;
  make_parse_test "ready with spaces" "    ready   " Ready;

  make_parse_exn_test "place with empty [object_phrase]" "place          "
    Malformed;
  make_parse_exn_test "shoot with empty [object_phrase]" "       shoot      "
    Malformed;
  make_parse_exn_test "incorrect verb" "play" Malformed;
  make_parse_exn_test "incorrect verb 2" "      Quit     game ship       "
    Malformed;
  make_parse_exn_test "empty string" "" Empty;
  make_parse_exn_test "empty string 2" "               " Empty;
  make_parse_exn_test "status with nonempty [object_phrase]" "status jhghjk" 
    Malformed;
  make_parse_exn_test "help with nonempty [object_phrase]" "    help    7 ok" 
    Malformed;
  make_parse_exn_test "quit with nonempty [object_phrase]" "  quit    game " 
    Malformed;
  make_parse_exn_test "ready with nonempty [object_phrase]" "  ready  set go " 
    Malformed;  
]

(** [make_board_op_exn_test name f board expected_exn] constructs an OUnit
    test named [name] that asserts [f board] raises the [expected_exn]. *)
let make_board_op_exn_test
    (name : string)
    (f : Board.t -> 'a)
    (board : Board.t)
    (expected_exn : exn) : test = 
  name >:: (fun _ -> 
      assert_raises expected_exn (fun () -> f board))

(** [make_no_exn_raised_test name f board] constructs an OUnit test
    named [name] that asserts that [f board] raises no exceptions. *)
let make_no_exn_raised_test
    (name : string)
    (f : Board.t -> 'a)
    (board : Board.t) = 
  name >:: (fun _ -> 
      assert_equal true ( try (f board |> ignore; true)
                          with | _ -> false )
    )

(** [make_equal_test name f v exp] constructs an OUnit test
    named [name] that asserts the quality of [exp] with [f v]. *)
let make_equal_test
    (name : string)
    (f : 'b -> 'a)
    (v : 'b)
    (expected_output : 'a) = 
  name >:: (fun _ -> 
      assert_equal expected_output (f v)
    )

(** [make_row_col_test name loc expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_value] with 
    [Board.row_col loc]. *)
let make_row_col_test
    (name : string)
    (loc : Command.location)
    (expected_output : int*int) = 
  name >:: (fun _ ->
      assert_equal expected_output (Helpers.row_col loc))

(** [[make_is_unshot_test name board loc] constructs an OUnit test
    named [name] that asserts the quality of [expected_value] with 
    [Board.is_unshot board loc]. *)
let make_is_unshot_test 
    (name : string)
    (board : Board.t)
    (loc : (int*int))
    (expected_output : bool) = 
  name >:: (fun _ ->
      assert_equal expected_output (Board.is_unshot board loc))

(** [make_no_exn_mines_test name board num_mines] constructs an OUnit test
    named [name] that asserts that [Board.place_mine board num_mines] raises 
    no exceptions. *)
let make_no_exn_mines_test 
    (name : string)
    (board : Board.t)
    (num_mines : int) = 
  name >:: (fun _ -> 
      assert_equal true ( try (Board.place_mine board num_mines |> ignore; true)
                          with | _ -> false ))

(* Some of these tests make Board.t break the "all ships have been placed"
   invariant. This couldn't happen during gameplay. *)
let bd1 = Board.init_board_default "fake name"
let () = Board.place "battleship" "b2" "e2" bd1

let bd2 = Board.init_board_default "fake name 2"
let _ = Board.shoot "a3" bd2

let bd_lose = Board.init_board_default "this player will lose"
let () = Board.(
    place "battleship" "a1" "a4" bd_lose;
    place "cruiser" "b1" "b2" bd_lose;
    place "carrier" "c1" "c5" bd_lose;
    place "destroyer" "d1" "d3" bd_lose;
    place "submarine" "e1" "e3" bd_lose;
  )
let shoot_locs b =
  List.fold_left (fun () loc -> Board.shoot loc b |> ignore) ()
let () = shoot_locs bd_lose [
    "a1"; "a2"; "a3"; "a4";
    "b1"; "b2";
    "c1"; "c2"; "c3"; "c4"; "c5";
    "d1"; "d2"; "d3";
    "e1"; "e2"; "e3"
  ] 

let bd3 = Board.init_board_default "fake name"
let () = Board.place_m_r "battleship" (1,1) (4,1) bd3

let bd4 = Board.init_board_default "fake name"
let () = Board.place "battleship" "b2" "e2" bd4

let bd_full = Board.init_board_default "all ships placed"
let () = Board.place "default" "" "" bd_full

let bd_full2 = Board.init_board_default "all ships placed"
let () = Board.place "default" "" "" bd_full2
let _ = Board.shoot "j10" bd_full2

let bd_full3 = Board.init_board_default "all ships placed"
let () = Board.place "default" "" "" bd_full3
let _ = Board.shoot "b1" bd_full3

let bd_full4 = Board.init_board_default "all ships placed"
let () = Board.place "default" "" "" bd_full4
let _ = Board.shoot "b1" bd_full4
let _ = Board.shoot "b2" bd_full4

let bd_full5 = Board.init_board_default "all ships placed"
let () = Board.place "default" "" "" bd_full5
let _ = Board.shoot "b1" bd_full5

let bd_full6 = Board.init_board_default "all ships placed"
let () = Board.place "default" "" "" bd_full6
let _ = Board.shoot "b1" bd_full6
let _ = Board.shoot "b2" bd_full6
let _ = Board.shoot "f5" bd_full6

let bd_full7 = Board.init_board_default "all ships placed"
let () = Board.place "default" "" "" bd_full7

let board_tests = [
  (* Board.row_col *)
  make_row_col_test "a5 is (0, 4)" "a5" (0, 4);
  make_row_col_test "h6 is (7, 7)" "h6" (7, 5);
  make_row_col_test "j10 is (9, 9)" "j10" (9, 9);
  make_row_col_test "a1 is (0, 0)" "a1" (0, 0);

  (* Board.init_board and Board.player_name *)
  make_equal_test "board player name" Board.player_name bd1 "fake name";
  make_equal_test "board player name" Board.player_name bd2 "fake name 2";
  make_equal_test "board player name" Board.player_name bd_lose 
    "this player will lose";

  (* Board.place *)
  make_board_op_exn_test "OverlappingShips"
    (Board.place "destroyer" "b1" "b3") bd1 Board.OverlappingShips;
  make_board_op_exn_test "InvalidLoc"
    (Board.place "destroyer" "b9" "b12") bd1 Board.OffBoard;
  make_board_op_exn_test "InvalidShipName"
    (Board.place "destroyor" "b9" "b12") bd1 Board.InvalidShipName;
  make_board_op_exn_test "WrongLength"
    (Board.place "destroyer" "b2" "b9") bd1 Board.WrongLength;
  make_board_op_exn_test "Misaligned"
    (Board.place "destroyer" "b2" "c7") bd1 Board.Misaligned;
  make_no_exn_raised_test "insertion indices are reversed"
    (Board.place "cruiser" "h5" "h4") bd2;

  (* Board.place_m_r *)
  make_board_op_exn_test "OverlappingShips (place_m_r)"
    (Board.place_m_r "destroyer" (1, 0) (1, 2)) bd3 Board.OverlappingShips;
  make_board_op_exn_test "InvalidShipName (place_m_r)"
    (Board.place_m_r "destroyor" (1, 8) (1, 11)) bd3 Board.InvalidShipName;
  make_no_exn_raised_test "insertion indices are reversed (place_m_r)"
    (Board.place_m_r "cruiser" (7, 4) (7, 3)) bd3;

  (* Board.did_lose *)
  make_equal_test "bd_lose has lost" Board.did_lose bd_lose true;
  make_equal_test "bd2 has not lost" Board.did_lose bd2 false;

  (* Board.shoot *)
  make_no_exn_raised_test "can shoot without error"
    (Board.shoot "a3") bd1; 
  make_board_op_exn_test "shoot shot location" (Board.shoot "a3")
    bd2 Board.DuplicateShot;
  make_board_op_exn_test "invalid shot location"
    (Board.shoot "a99") bd1 Helpers.InvalidLoc;

  (* Board.shoot_m_r *)
  make_no_exn_raised_test "can shoot without error"
    (Board.shoot_m_r (2, 5)) bd1; 
  make_board_op_exn_test "shoot shot location" (Board.shoot_m_r (0, 2))
    bd2 Board.DuplicateShot;
  make_board_op_exn_test "invalid shot location"
    (Board.shoot_m_r (3, 98)) bd1 Helpers.InvalidLoc;
  make_equal_test "successful sunk ship" (Board.shoot_m_r (1, 1))
    bd_full5 (true, true);

  (* Board.setup_status *)
  make_equal_test "fully set up board" Board.setup_status bd_full (
    "On the board: battleship (length 4); cruiser (length 2); carrier " ^
    "(length 5); destroyer (length 3); submarine (length 3)\nOff "^
    "the board: None");
  make_equal_test "partially set up board" Board.setup_status bd4 (
    "On the board: battleship (length 4)\nOff the board: "^
    "cruiser (length 2); carrier (length 5); destroyer (length 3); " ^
    "submarine (length 3)");

  (* Board.setup_status_m_r *)
  make_equal_test "fully set up board" Board.setup_status_m_r bd_full [];
  make_equal_test "partially set up board" Board.setup_status_m_r bd4 
    [("cruiser", 2); ("carrier", 5); ("destroyer", 3); ("submarine", 3)];

  (* Board.status *)
  make_equal_test "no shots taken" Board.status bd_full 
    "You still have ships left. ";
  make_equal_test "shot taken, opponent missed" Board.status bd_full2 
    "You still have ships left. Your opponent shot J10 and missed.";
  make_equal_test "shot taken, opponent hit cruiser" Board.status bd_full3 
    "You still have ships left. Your opponent shot your cruiser.";
  make_equal_test "shot taken, opponent sunk cruiser" Board.status bd_full4 
    "You still have ships left. Your opponent sank your cruiser.";

  (* Board.complete *)
  make_equal_test "incomplete" Board.complete bd1 false;
  make_equal_test "complete" Board.complete bd_full true;

  (* Board.is_part_of_living_ship *)
  make_equal_test "alive ship" (Board.is_part_of_living_ship bd_full)
    (1, 1) true;
  make_equal_test "dead ship" (Board.is_part_of_living_ship bd_full4)
    (1, 0) false;
  make_equal_test "no ship" (Board.is_part_of_living_ship bd_full5 )
    (9, 9) false;

  (* Board.string_self *)
  make_equal_test "every type of spot" (Board.string_self) bd_full6
    [["-"; "-"; "-"; "-"; "w"; "w"; "w"; "w"; "w"; "w"];
     ["#"; "#"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"];
     ["w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"];
     ["w"; "-"; "-"; "-"; "-"; "-"; "w"; "w"; "w"; "w"];
     ["w"; "|"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"];
     ["w"; "|"; "w"; "-"; "X-"; "-"; "w"; "w"; "w"; "w"];
     ["w"; "|"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"];
     ["w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"];
     ["w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"];
     ["w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"; "w"]];

  (* Board.string_other *)
  make_equal_test "every type of spot" (Board.string_other) bd_full6
    [["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["#"; "#"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "X-"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"];
     ["?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"; "?"]];

  (* Board.is_unshot *)
  make_is_unshot_test "not shot water" bd_full2 (9,8) true;
  make_is_unshot_test "not shot ship" bd_full2 (1,0) true;
  make_is_unshot_test "shot water" bd_full2 (9,9) false;
  make_is_unshot_test "shot water" bd_full5 (1,0) false;

  (* Board.place_mine *)
  make_no_exn_mines_test "place 10" bd_full7 10;
]

(** [make_no_exn_raised_test name f ai_board] constructs an OUnit test
    named [name] that asserts that [f ai_board] raises no exceptions. *)
let make_no_exn_raised_ai_place_test 
    (name : string)
    (f : 'a -> 'b)
    (ai_board : 'a) = 
  name >:: (fun _ ->
      assert_equal true ( try (f ai_board |> ignore; true)
                          with | _ -> false ))

(** [make_shoot_ship_ai_test name shoot_funct player_board expected_output] 
    constructs an OUnit test named [name] constructs an OUnit test
    named [name] that asserts that [shoot_funct player_board] raises
    no exceptions. *)
let make_no_exn_raised_ai_shoot_test
    (name : string)
    (shoot_funct : Board.t -> string)
    (player_board : Board.t) = 
  name >:: (fun _ ->
      assert_equal true ( try (shoot_funct player_board |> ignore; true)
                          with | _ -> false ))

(** [make_shoot_ship_ai_test name shoot_funct player_board expected_output] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [shoot_funct player_board]. *)
let make_ai_get_board_test
    (name : string)
    (get_board_funct : 'a -> Board.t)
    (ai_board : 'a)
    (expected_output : Board.t) = 
  name >:: (fun _ ->
      assert_equal expected_output (get_board_funct ai_board))

let ai_rand1 = Ai_random.init()
let ai_norm1 = Ai_normal.init()
let ai_smart1 = Ai_smart.init()

let ai_rand2 = Ai_random.init()
let p_rand2 = Board.init_board_default "💻"

let ai_norm2 = Ai_normal.init()
let p_norm2 = Board.init_board_default "💻"

let ai_smart2 = Ai_smart.init()
let p_smart2 = Board.init_board_default "💻"

let p1 = Board.init_board_default "player 1"

let ai_tests = [
  (* Ai.init and Ai.place_all_ships *)
  make_no_exn_raised_ai_place_test "place all ships ai random" 
    (Ai_random.place_all_ships) ai_rand1;
  make_no_exn_raised_ai_place_test "place all ships ai random" 
    (Ai_normal.place_all_ships) ai_norm1;
  make_no_exn_raised_ai_place_test "place all ships ai random" 
    (Ai_smart.place_all_ships) ai_smart1;

  (* Ai.shoot_ship *)
  make_no_exn_raised_ai_shoot_test "shoot ai random" 
    (Ai_random.shoot_ship ai_rand1) p1;
  make_no_exn_raised_ai_shoot_test "shoot ai normal" 
    (Ai_normal.shoot_ship ai_norm1) p1;
  make_no_exn_raised_ai_shoot_test "shoot ai smart" 
    (Ai_smart.shoot_ship ai_smart1) p1;

  (* Ai.get_board *)
  make_ai_get_board_test "get board ai random" 
    (Ai_random.get_board) ai_rand2 p_rand2;
  make_ai_get_board_test "get board ai normal" 
    (Ai_normal.get_board) ai_norm2 p_norm2;
  make_ai_get_board_test "get board ai smart" 
    (Ai_smart.get_board) ai_smart2 p_smart2  
]

(** [make_helper_rrc_test name coord expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected output] with 
    [rev_row_col coord]. *)
let make_helper_rrc_test
    (name : string)
    (coord : int * int)
    (expected_output : string) =
  name >:: (fun _ ->
      assert_equal (rev_row_col coord) expected_output)

(** [make_helper_ff_test name key expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected output] with 
    [from_file key]. *)
let make_helper_ff_test
    (name : string)
    (key : string)
    (expected_output : string) =
  name >:: (fun _ ->
      assert_equal (from_file key) expected_output)

let helper_tests = [
  make_helper_rrc_test "Test a1" (0, 0) "A1";
  make_helper_rrc_test "Test b2" (1, 1) "B2";

  make_helper_ff_test "Test load screen" "load_screen_a" 
    ("---------------" ^
     "\n    //\\\\       \n   //  \\\\      \n  //____\\\\     \n //      \\\\"^
     "    \n//        \\\\   ");
  make_helper_ff_test "Test error" "main_offboard" 
    ("\n\nYou cannot place the ship there.\nPlease enter coordinates that" ^
     " are on the board.");

  make_exn_raised_test "offboard in helpers" Helpers.row_col "AAA"
    Helpers.InvalidLoc;

]

(** [make_gbf_test name file expected_ouput] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with 
    [get_board_from_file file]. *)
let make_gbf_test 
    (name : string)
    (file : string)
    (expected_output : int * string * (string * int) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_board_from_file file))

let command_parser_tests = 
  [
    make_gbf_test "space board" "custom_boards/example.json" 
      (12, "space", [("klingons", 4); ("destroyer", 2); ("punisher", 5)]);
    make_gbf_test "pasta board" "custom_boards/example2.json" 
      (12, "", 
       [("fettuccine", 4); ("penne", 2); ("spaghetti", 9); ("linguini", 4)])
  ]

let suite =
  "test suite for game engine"  >::: List.flatten [
    command_tests;
    board_tests;
    ai_tests;
    helper_tests;
    command_parser_tests
  ]

let _ = run_test_tt_main suite