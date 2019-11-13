open OUnit2
open Command
open Board

(** [make_parse_test name str_input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with 
    [parse str_input]. *)
let make_parse_test 
    (name : string)
    (str_input : string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str_input))

(** [make_parse_exn_test name str_input expected_exn] constructs an OUnit test 
    named [name] that asserts [parse str_unput] raises the [expected_exn]. *)
let make_parse_exn_test
    (name : string)
    (str_input : string)
    (expected_exn : exn) : test = 
  name >:: (fun _ -> 
      assert_raises expected_exn (fun () -> parse str_input))

let command_tests = [
  make_parse_test "normal place" "place ship on shot ship" 
    (Place ["ship"; "shot"; "ship"]);

  make_parse_test "normal shoot" "shoot Ship" 
    (Shoot ["Ship"]);
  make_parse_test "shoot with numbers and spaces" " shoot 6  " 
    (Shoot ["6"]);

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
    named [name] that asserts the quality of [()] with 
    [f board]. *)
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
      assert_equal expected_output (Board.row_col loc))

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

(* Some of these tests make Board.t break the "all ships have been placed"
   invariant. This couldn't happen during gameplay. *)
let bd1 = Board.init_board "fake name"
let () = Board.place "battleship" "b2" "e2" bd1

let bd2 = Board.init_board "fake name 2"
let _ = Board.shoot "a3" bd2

let bd_lose = Board.init_board "this player will lose"
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

let bd3 = Board.init_board "fake name"
let () = Board.place_m_r "battleship" (1,1) (4,1) bd3

let bd4 = Board.init_board "fake name"
let () = Board.place "battleship" "b2" "e2" bd4

let bd_full = Board.init_board "all ships placed"
let () = Board.place "default" "" "" bd_full

let bd_full2 = Board.init_board "all ships placed"
let () = Board.place "default" "" "" bd_full2
let _ = Board.shoot "j10" bd_full2

let bd_full3 = Board.init_board "all ships placed"
let () = Board.place "default" "" "" bd_full3
let _ = Board.shoot "b1" bd_full3

let bd_full4 = Board.init_board "all ships placed"
let () = Board.place "default" "" "" bd_full4
let _ = Board.shoot "b1" bd_full4
let _ = Board.shoot "b2" bd_full4

let bd_full5 = Board.init_board "all ships placed"
let () = Board.place "default" "" "" bd_full5
let _ = Board.shoot "b1" bd_full5

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
    (Board.shoot "a99") bd1 Board.InvalidLoc;

  (* Board.shoot_m_r *)
  make_no_exn_raised_test "can shoot without error"
    (Board.shoot_m_r (2, 5)) bd1; 
  make_board_op_exn_test "shoot shot location" (Board.shoot_m_r (0, 2))
    bd2 Board.DuplicateShot;
  make_board_op_exn_test "invalid shot location"
    (Board.shoot_m_r (3, 98)) bd1 Board.InvalidLoc;
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

  (* Board.is_unshot*)
  make_is_unshot_test "not shot water" bd_full2 (9,8) true;
  make_is_unshot_test "not shot ship" bd_full2 (1,0) true;
  make_is_unshot_test "shot water" bd_full2 (9,9) false;
  make_is_unshot_test "shot water" bd_full5 (1,0) false;

]


let suite =
  "test suite for game engine"  >::: List.flatten [
    command_tests;
    board_tests
  ]

let _ = run_test_tt_main suite