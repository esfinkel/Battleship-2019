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
  make_parse_test "remove with spaces" "     remove     ship     "
    (Remove ["ship"]);
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
  make_parse_exn_test "remove with empty [object_phrase]" "          remove"
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

let board_tests = [

]

let suite =
  "test suite for A2"  >::: List.flatten [
    command_tests;
    board_tests
  ]

let _ = run_test_tt_main suite