open OUnit2
open TestUtils
open TestInputs

let interp_iter = List.iter test_interp
let optimize_iter = List.iter test_optimize


let test_var _ctxt =
  interp_iter var_list;
  optimize_iter var_list

let test_randp _ctxt =
  for _ = 1 to 100 do
    let n = Utils.Generator.depth () in
    let randp = randp n in
    test_interp randp;
    test_optimize randp
  done


let suite =
  "r1_tests" >::: [
    "var" >:: test_var;
    "randp" >:: test_randp
  ]

let _ = run_test_tt_main suite

