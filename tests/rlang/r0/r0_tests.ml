open OUnit2
open TestUtils
open TestInputs

let interp_iter = List.iter test_interp

let optimize_iter = List.iter test_optimize

let test_int _ctxt =
  interp_iter int_list;
  optimize_iter int_list

let test_read _ctxt =
  interp_iter read_list;
  optimize_iter read_list

let test_add _ctxt =
  interp_iter add_list;
  optimize_iter add_list

let test_negate _ctxt =
  interp_iter negate_list;
  optimize_iter negate_list

let test_randp _ctxt =
  for _ = 1 to 100 do
    let n = R_utils.Generator.depth () in
    let randp = randp n in
    test_interp randp;
    test_optimize randp
  done

let suite =
  "r0_tests"
  >::: [ "int" >:: test_int
       ; "read" >:: test_read
       ; "add" >:: test_add
       ; "negate" >:: test_negate
       ; "randp" >:: test_randp
       ]

let _ = run_test_tt_main suite
