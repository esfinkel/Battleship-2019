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

(** [make_board_op_exn_test name f board expected_exn] constructs an OUnit test 
    named [name] that asserts [f board] raises the [expected_exn]. *)
let make_board_op_exn_test
    (name : string)
    (f : Board.t -> 'a)
    (board : Board.t)
    (expected_exn : exn) : test = 
  name >:: (fun _ -> 
      assert_raises expected_exn (fun () -> f board))

(** [make_no_exn_raised_test name f board] constructs an OUnit test
    named [name] that asserts the quality of [())] with 
    [f board]. *)
let make_no_exn_raised_test
    (name : string)
    (f : Board.t -> 'a)
    (board : Board.t) = 
  name >:: (fun _ -> 
      assert_equal true ( try (f board |> ignore; true)
                          with | _ -> false )
    )


let make_equal_test
    (name : string)
    (f : Board.t -> 'a)
    (board : Board.t)
    (expected_output : 'a) = 
  name >:: (fun _ -> 
      assert_equal expected_output (f board)
    )


let command_tests = [
  make_parse_test "normal place" "place ship on shot ship" 
    (Place ["ship"; "shot"; "ship"]);

  make_parse_test "normal shoot" "shoot Ship" 
    (Shoot ["Ship"]);
  make_parse_test "shoot with numbers and spaces" " shoot 6  " 
    (Shoot ["6"]);

  make_parse_test "normal quit" "quit" Quit;
  make_parse_test "quit with spaces" "    quit   " Quit;

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

let bd_full = Board.init_board "all ships placed"
let () = Board.place "default" "" "" bd_full

let board_tests = [
  make_equal_test "board player name" Board.player_name bd1 "fake name";

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

  make_equal_test "incomplete" Board.complete bd1 false;
  make_equal_test "complete" Board.complete bd_full true;

  make_no_exn_raised_test "can shoot without error"
    (Board.shoot "a3") bd1; 
  make_board_op_exn_test "shoot shot location" (Board.shoot "a3")
    bd2 Board.DuplicateShot;
  make_board_op_exn_test "invalid shot location"
    (Board.shoot "a99") bd1 Board.InvalidLoc;

  make_equal_test "bd_lose has lost" Board.did_lose bd_lose true;
  make_equal_test "bd2 has not lost" Board.did_lose bd2 false;

]


let suite =
  "test suite for A2"  >::: List.flatten [
    command_tests;
    board_tests
  ]

let _ = run_test_tt_main suite