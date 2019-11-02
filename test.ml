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
  (* make_parse_test "remove with spaces" "     remove     ship     "
     (Remove ["ship"]); *)
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
  (* make_parse_exn_test "remove with empty [object_phrase]" "          remove"
     Malformed; *)
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


(* init_board  *)
let b1 = Board.init_board "fake name"
let b2 = Board.init_board "fake name 2"
let () = Board.place "battleship" "b2" "e2" b1
let _ = Board.shoot "a3" b2

let board_tests = [

  (* assert (Board.player_name b1 = "fake name") *)
  make_board_op_exn_test "OverlappingShips"
    (Board.place "destroyer" "b1" "b3") b1 Board.OverlappingShips;
  make_board_op_exn_test "InvalidLoc"
    (Board.place "destroyer" "b9" "b12") b1 Board.OffBoard;
  make_board_op_exn_test "InvalidShipName"
    (Board.place "destroyor" "b9" "b12") b1 Board.InvalidShipName;
  make_board_op_exn_test "WrongLength"
    (Board.place "destroyer" "b2" "b9") b1 Board.WrongLength;
  make_board_op_exn_test "Misaligned"
    (Board.place "destroyer" "b2" "c7") b1 Board.Misaligned;



  (* make setup_status b1 test - only battleship is on the grid *)

  (* make test for offboard *)
  (* make test for misaligned *)
  (* make test for wronglength *)
  (* make one test with make_no_exn_raised_test (and l1 > l2) *)

  make_equal_test "incomplete" Board.complete b1 false;


  (* insert the other ships *)


  (* make_equal_test "complete" Board.complete b1 true; *)

  (* we will test shoot by gameplay. unless someone wants to  *)
  make_no_exn_raised_test "can shoot without error"
    (Board.shoot "a3") b1; 

  make_equal_test "b1 has not lost" Board.status b1
    "You still have ships left. (Just a placeholder for now)";

  make_board_op_exn_test "shoot shot location" (Board.shoot "a3")
    b2 Board.DuplicateShot;
  make_board_op_exn_test "invalid shot location"
    (Board.shoot "a99") b1 Board.InvalidLoc;

  (* shoot remaining ships *)

  (* make_equal_test "b1 has lost" Board.status b1
     "All of your ships have been destroyed."; *)

  (* let's just test string_self and string_other by inspection, unless
     someone really wants to make the grid *)


]


let suite =
  "test suite for A2"  >::: List.flatten [
    command_tests;
    board_tests
  ]

let _ = run_test_tt_main suite