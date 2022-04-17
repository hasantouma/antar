open OUnit2
open TestUtils
open Rlang
open Clang
open Passes

let econ1 = make_rprog (EInt 5)
let econ1' = wrap_c_entry (Return (Number 5))
let econ2 = make_rprog (ENegate (EInt 6))
let econ2' = wrap_c_entry (Seq (Set ("x0", Negate (Number 6)), Return (Var "x0")))
let econ3 = make_rprog (ELet ("x", ERead, EAdd (EInt 2, EVar "x")))
let econ3' = wrap_c_entry (Seq (Set ("x", Read), Seq (Set ("x0", Add (Number 2, Var "x")), Return (Var "x0"))))

let econ4 =
  make_rprog (ELet ("a", EInt 42, ELet ("x", ENegate (EVar "a"), ELet ("y", ERead, EAdd (EVar "x", EVar "y")))))

let econ4' =
  wrap_c_entry
    (Seq
       ( Set ("a", Arg (Number 42))
       , Seq
           ( Set ("x", Negate (Var "a"))
           , Seq (Set ("y", Read), Seq (Set ("x0", Add (Var "x", Var "y")), Return (Var "x0"))) ) ))

let econ5 = make_rprog (ELet ("x", ERead, ENegate (EVar "x")))
let econ5' = wrap_c_entry (Seq (Set ("x", Read), Seq (Set ("y", Negate (Var "x")), Return (Var "y"))))

let econ6 =
  make_rprog
    (ELet ("x", ELet ("y", ERead, ELet ("z", ENegate (EInt 42), EAdd (EVar "y", EVar "z"))), ENegate (EVar "x")))

let econ6' =
  wrap_c_entry
    (Seq
       ( Set ("y", Read)
       , Seq
           ( Set ("z", Negate (Number 42))
           , Seq (Set ("x", Add (Var "y", Var "z")), Seq (Set ("x0", Negate (Var "x")), Return (Var "x0"))) ) ))

let econ7 = make_rprog (ELet ("x", ELet ("y", ENegate (EInt 10), EAdd (EInt 42, EVar "y")), EAdd (EVar "x", EInt 10)))

let econ7' =
  wrap_c_entry
    (Seq
       ( Set ("y", Negate (Number 10))
       , Seq (Set ("x", Add (Number 42, Var "y")), Seq (Set ("x1", Add (Var "x", Number 10)), Return (Var "x1"))) ))

let test_explicate_control ctxt =
  assert_equal econ1' (explicate_control econ1) ~cmp:clang_alpha_equiv ~msg:"econ1" ~printer:Clang.pp;
  assert_equal econ2' (explicate_control econ2) ~cmp:clang_alpha_equiv ~msg:"econ2" ~printer:Clang.pp;
  assert_equal econ3' (explicate_control econ3) ~cmp:clang_alpha_equiv ~msg:"econ3" ~printer:Clang.pp;
  assert_equal econ4' (explicate_control econ4) ~cmp:clang_alpha_equiv ~msg:"econ4" ~printer:Clang.pp;
  assert_equal econ5' (explicate_control econ5) ~cmp:clang_alpha_equiv ~msg:"econ5" ~printer:Clang.pp;
  assert_equal econ6' (explicate_control econ6) ~cmp:clang_alpha_equiv ~msg:"econ6" ~printer:Clang.pp;
  assert_equal econ7' (explicate_control econ7) ~cmp:clang_alpha_equiv ~msg:"econ7" ~printer:Clang.pp

let test_is_resolve_complex ctxt =
  assert_equal true (is_resolve_complex econ1) ~msg:"is_resolve_complex: econ1" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex econ2) ~msg:"is_resolve_complex: econ2" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex econ3) ~msg:"is_resolve_complex: econ3" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex econ4) ~msg:"is_resolve_complex: econ4" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex econ5) ~msg:"is_resolve_complex: econ5" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex econ6) ~msg:"is_resolve_complex: econ6" ~printer:string_of_bool;
  assert_equal true (is_resolve_complex econ7) ~msg:"is_resolve_complex: econ7" ~printer:string_of_bool

let test_interp_explicate_control ctxt =
  assert_equal (Clang.interp econ1') (Rlang.interp econ1) ~msg:"interp_explicate_control: econ1" ~printer:string_of_int;
  assert_equal (Clang.interp econ2') (Rlang.interp econ2) ~msg:"interp_explicate_control: econ2" ~printer:string_of_int;
  assert_equal (Clang.interp ~inputs:[ 42 ] econ3') (Rlang.interp ~inputs:[ 42 ] econ3)
    ~msg:"interp_explicate_control: econ3" ~printer:string_of_int;
  assert_equal (Clang.interp ~inputs:[ 42 ] econ4') (Rlang.interp ~inputs:[ 42 ] econ4)
    ~msg:"interp_explicate_control: econ4" ~printer:string_of_int;
  assert_equal (Clang.interp ~inputs:[ 42 ] econ5') (Rlang.interp ~inputs:[ 42 ] econ5)
    ~msg:"interp_explicate_control: econ5" ~printer:string_of_int;
  assert_equal (Clang.interp ~inputs:[ 42 ] econ6') (Rlang.interp ~inputs:[ 42 ] econ6)
    ~msg:"interp_explicate_control: econ6" ~printer:string_of_int;
  assert_equal (Clang.interp econ7') (Rlang.interp econ7) ~msg:"interp_explicate_control: econ7" ~printer:string_of_int

let suite =
  "explicate_control_tests"
  >::: [ "explicate_control" >:: test_explicate_control
       ; "is_resolve_complex" >:: test_is_resolve_complex
       ; "interp_explicate_control" >:: test_interp_explicate_control
       ]

let _ = run_test_tt_main suite
