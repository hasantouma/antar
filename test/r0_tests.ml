open OUnit2
open TestUtils
open TestInputs

let test_int _ctxt =
  test_interp one;
  test_interp fourty_two


let test_read _ctxt =
  test_interp read


let test_add _ctxt =
  test_interp add_1_2;
  test_interp add_33_read;
  test_interp add_read_72;
  test_interp add_read_read


let test_negate _ctxt =
  test_interp negate_1;
  test_interp negate_read;
  test_interp add_read_and_negate_72;
  test_interp negate_add_99_50;
  test_interp negate_negate_5

let test_randp _ctxt =
  for _ = 1 to 100 do
    let randp3 = randp3 () in
    test_interp randp3
  done


let suite =
  "r0_tests" >::: [
    "int" >:: test_int;
    "read" >:: test_read;
    "add" >:: test_add;
    "negate" >:: test_negate;
    "randp" >:: test_randp
  ]

let _ = run_test_tt_main suite

