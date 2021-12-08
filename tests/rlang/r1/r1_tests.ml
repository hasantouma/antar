open OUnit2
open TestUtils
open TestInputs

let interp_iter = List.iter test_interp

let compiler_iter = List.iter test_compiler

let optimize_iter = List.iter test_optimize

let test_var _ctxt =
  interp_iter var_list;
  compiler_iter var_list;
  optimize_iter var_list

let test_let _ctxt =
  interp_iter let_list;
  compiler_iter let_list;
  optimize_iter let_list

let test_randp _ctxt =
  for _ = 1 to 100 do
    let n = Utils.Generator.depth () in
    let randp = randp n in
    test_interp randp;
    test_compiler randp;
    test_optimize randp
  done

let suite = "r1_tests" >::: [ "var" >:: test_var; "let" >:: test_let; "randp" >:: test_randp ]

let _ = run_test_tt_main suite
