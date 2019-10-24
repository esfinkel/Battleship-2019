open OUnit2
open Command

(** [make_parse_test name str_input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with 
    [parse str_input]. *)
let make_parse_test 
    (name : string)
    (str_input : string)
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str_input))

let command_tests = [
  make_parse_test "normal place" "place ship board shot ship" 
    (Place ["ship"; "board"; "shot"; "ship"]);
]

let board_tests = [

]


let suite =
  "test suite for A2"  >::: List.flatten [
    command_tests;
    board_tests
  ]

let _ = run_test_tt_main suite