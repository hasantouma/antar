open OUnit2
open TestUtils
open Passes
open Rlang

let rco1 = make_rprog (EInt 5)
let rco1' = rco1
let rco2 = make_rprog (ENegate (EInt 6))
let rco2' = rco2
let rco3 = make_rprog (EAdd (EInt 2, ERead))
let rco3' = { rco3 with e = ELet ("x0", ERead, EAdd (EInt 2, EVar "x0")) }
let rco4 = make_rprog (EAdd (ENegate (EInt 42), ERead))
let rco4' = { rco4 with e = ELet ("x0", ENegate (EInt 42), ELet ("x1", ERead, EAdd (EVar "x0", EVar "x1"))) }
let rco5 = make_rprog (ENegate ERead)
let rco5' = { rco5 with e = ELet ("x1", ERead, ENegate (EVar "x1")) }
let rco6 = make_rprog (ENegate (EAdd (ERead, ENegate (EInt 42))))

let rco6' =
  { rco6 with
    e = ELet ("x0", ELet ("x1", ERead, ELet ("x2", ENegate (EInt 42), EAdd (EVar "x1", EVar "x2"))), ENegate (EVar "x0"))
  }

let rco7 = make_rprog (ELet ("x", EAdd (EInt 42, ENegate (EInt 10)), EAdd (EVar "x", EInt 10)))

let rco7' =
  { rco7 with e = ELet ("x", ELet ("x0", ENegate (EInt 10), EAdd (EInt 42, EVar "x0")), EAdd (EVar "x", EInt 10)) }

let test_resolve_complex ctxt =
  assert_equal rco1' (resolve_complex rco1) ~cmp:rlang_alpha_equiv ~msg:"resolve_complex: rco1" ~printer:pp;
  assert_equal rco2' (resolve_complex rco2) ~cmp:rlang_alpha_equiv ~msg:"resolve_complex: rco2" ~printer:pp;
  assert_equal rco3' (resolve_complex rco3) ~cmp:rlang_alpha_equiv ~msg:"resolve_complex: rco3" ~printer:pp;
  assert_equal rco4' (resolve_complex rco4) ~cmp:rlang_alpha_equiv ~msg:"resolve_complex: rco4" ~printer:pp;
  assert_equal rco5' (resolve_complex rco5) ~cmp:rlang_alpha_equiv ~msg:"resolve_complex: rco5" ~printer:pp;
  assert_equal rco6' (resolve_complex rco6) ~cmp:rlang_alpha_equiv ~msg:"resolve_complex: rco6" ~printer:pp;
  assert_equal rco7' (resolve_complex rco7) ~cmp:rlang_alpha_equiv ~msg:"resolve_complex: rco7" ~printer:pp

let test_is_uniquify ctxt =
  assert_equal true (is_uniquify rco1) ~msg:"resolve_complex: is_uniquify: rco1" ~printer:string_of_bool;
  assert_equal true (is_uniquify rco2) ~msg:"resolve_complex: is_uniquify: rco2" ~printer:string_of_bool;
  assert_equal true (is_uniquify rco3) ~msg:"resolve_complex: is_uniquify: rco3" ~printer:string_of_bool;
  assert_equal true (is_uniquify rco4) ~msg:"resolve_complex: is_uniquify: rco4" ~printer:string_of_bool;
  assert_equal true (is_uniquify rco5) ~msg:"resolve_complex: is_uniquify: rco5" ~printer:string_of_bool;
  assert_equal true (is_uniquify rco6) ~msg:"resolve_complex: is_uniquify: rco6" ~printer:string_of_bool;
  assert_equal true (is_uniquify rco7) ~msg:"resolve_complex: is_uniquify: rco7" ~printer:string_of_bool

let test_is_resolve_complex ctxt =
  assert_equal true (is_resolve_complex rco1') ~msg:"is_resolve_complex: rco1" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex rco2') ~msg:"is_resolve_complex: rco2" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex rco3') ~msg:"is_resolve_complex: rco3" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex rco4') ~msg:"is_resolve_complex: rco4" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex rco5') ~msg:"is_resolve_complex: rco5" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex rco6') ~msg:"is_resolve_complex: rco6" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex rco7') ~msg:"is_resolve_complex: rco7" ~printer:string_of_bool

let test_interp_resolve_complex ctxt =
  assert_equal (interp rco1') (interp rco1) ~msg:"interp_resolve_complex: rco1" ~printer:string_of_int;
  assert_equal (interp rco2') (interp rco2) ~msg:"interp_resolve_complex: rco2" ~printer:string_of_int;
  assert_equal (interp ~inputs:[ 42 ] rco3') (interp ~inputs:[ 42 ] rco3) ~msg:"interp_resolve_complex: rco3"
    ~printer:string_of_int;
  assert_equal (interp ~inputs:[ 42 ] rco4') (interp ~inputs:[ 42 ] rco4) ~msg:"interp_resolve_complex: rco4"
    ~printer:string_of_int;
  assert_equal (interp ~inputs:[ 42 ] rco5') (interp ~inputs:[ 42 ] rco5) ~msg:"interp_resolve_complex: rco5"
    ~printer:string_of_int;
  assert_equal (interp ~inputs:[ 42 ] rco6') (interp ~inputs:[ 42 ] rco6) ~msg:"interp_resolve_complex: rco6"
    ~printer:string_of_int;
  assert_equal (interp rco7') (interp rco7) ~msg:"interp_resolve_complex: rco7" ~printer:string_of_int

let suite =
  "resolve_complex_tests"
  >::: [ "resolve_complex" >:: test_resolve_complex
       ; "is_uniquify" >:: test_is_uniquify
       ; "resolve_is_complex" >:: test_is_resolve_complex
       ; "interp_resolve_complex" >:: test_interp_resolve_complex
       ]

let _ = run_test_tt_main suite
