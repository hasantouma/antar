open OUnit2
open String
open Pretty_print

let test_int _ctxt =
  assert_equal "1"  (pp (`EInt 1))  ~msg:"int (1)" ~printer:lowercase_ascii;
  assert_equal "42" (pp (`EInt 42)) ~msg:"int (2)" ~printer:lowercase_ascii

let test_read _ctxt =
  assert_equal "(read)" (pp (`ERead)) ~msg:"read (1)" ~printer:lowercase_ascii

let test_add _ctxt =
  assert_equal "(+ 1 2)"       (pp (`EAdd (`EInt 1, `EInt 2))) ~msg:"add (1)" ~printer:lowercase_ascii;
  assert_equal "(+ 33 (read))" (pp (`EAdd (`EInt 33, `ERead))) ~msg:"add (2)" ~printer:lowercase_ascii;
  assert_equal "(+ (read) 72)" (pp (`EAdd (`ERead, `EInt 72))) ~msg:"add (3)" ~printer:lowercase_ascii

let test_negate _ctxt =
  assert_equal "(- 1)"             (pp (`ENegate (`EInt 1)))                      ~msg:"negate (1)" ~printer:lowercase_ascii;
  assert_equal "(- (read))"        (pp (`ENegate (`ERead)))                       ~msg:"negate (2)" ~printer:lowercase_ascii;
  assert_equal "(+ (read) (- 72))" (pp (`EAdd (`ERead, `ENegate (`EInt 72))))     ~msg:"negate (3)" ~printer:lowercase_ascii;
  assert_equal "(- (+ 99 50))"     (pp (`ENegate (`EAdd (`EInt 99, `EInt 50))))   ~msg:"negate (4)" ~printer:lowercase_ascii

let suite =
  "r0_tests" >::: [
    "int" >:: test_int;
    "read" >:: test_read;
    "add" >:: test_add
  ]

let _ = run_test_tt_main suite

