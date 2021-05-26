open OUnit2
open TestUtils
open TestInputs

let test_int _ctxt =
  test_pp     one;
  test_interp one;
  
  test_pp     fourty_two;
  test_interp fourty_two


let test_read _ctxt =
  test_pp     read;
  test_interp read


let test_add _ctxt =
  test_pp     add_1_2;
  test_interp add_1_2;
  
  test_pp     add_33_read;
  test_interp add_33_read;

  test_pp     add_read_72;
  test_interp add_read_72


let test_negate _ctxt =
  test_pp     negate_1;
  test_interp negate_1;

  test_pp     negate_read;
  test_interp negate_read;

  test_pp     add_read_and_negate_72;
  test_interp add_read_and_negate_72;

  test_pp     negate_add_99_50;
  test_interp negate_add_99_50;

  test_pp     negate_negate_5;
  test_interp negate_negate_5



let suite =
  "r0_tests" >::: [
    "int" >:: test_int;
    "read" >:: test_read;
    "add" >:: test_add;
    "negate" >:: test_negate
  ]

let _ = run_test_tt_main suite

