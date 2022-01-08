open OUnit2
open TestUtils
open TestInputs

let interp_iter = List.iter test_interp
let compiler_iter = List.iter test_compiler

let test_int _ctxt =
  interp_iter int_list;
  compiler_iter int_list

let test_read _ctxt =
  interp_iter read_list;
  compiler_iter read_list

let test_add _ctxt =
  interp_iter add_list;
  compiler_iter add_list

let test_negate _ctxt =
  interp_iter negate_list;
  compiler_iter negate_list

let test_var _ctxt =
  interp_iter var_list;
  compiler_iter var_list

let test_let _ctxt =
  interp_iter let_list;
  compiler_iter let_list

let test_randp _ctxt =
  for _ = 1 to 100 do
    let n = Utils.Generator.depth () in
    let randp = randp n in
    test_interp randp;
    test_compiler randp
  done

let suite =
  "rlang_tests"
  >::: [ "int" >:: test_int
       ; "read" >:: test_read
       ; "add" >:: test_add
       ; "negate" >:: test_negate
       ; "var" >:: test_var
       ; "let" >:: test_let
       ; "randp" >:: test_randp
       ]

let _ = run_test_tt_main suite
