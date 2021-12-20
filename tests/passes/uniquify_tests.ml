open OUnit2
open Passes.Uniquify
open R1.Ast
open R1.Interp

let u1 = ELet ("a", ERead, EVar "a")
let u1' = ELet ("x0", ERead, EVar "x0")
let u2 = ELet ("a", ELet ("a", ENegate (EInt 3), EAdd (EVar "a", EVar "a")), EVar "a")
let u2' = ELet ("x0", ELet ("x1", ENegate (EInt 3), EAdd (EVar "x1", EVar "x1")), EVar "x0")
let u3 = ELet ("a", EInt 4, ELet ("a", ENegate (EInt 3), EAdd (EVar "a", EVar "a")))
let u3' = ELet ("x0", EInt 4, ELet ("x1", ENegate (EInt 3), EAdd (EVar "x1", EVar "x1")))

let u4 =
  EAdd (ELet ("x", EInt 7, EVar "x"), ELet ("x", EInt 8, ELet ("x", EAdd (EInt 1, EVar "x"), EAdd (EVar "x", EVar "x"))))

let u4' =
  EAdd
    ( ELet ("x0", EInt 7, EVar "x0")
    , ELet ("x1", EInt 8, ELet ("x2", EAdd (EInt 1, EVar "x1"), EAdd (EVar "x2", EVar "x2"))) )

let test_uniquify _ctxt =
  assert_equal u1' (uniquify u1) ~cmp:TestUtils.rlang_alpha_equiv ~msg:"uniquify: u1" ~printer:R1.Pp.pp;
  assert_equal u2' (uniquify u2) ~cmp:TestUtils.rlang_alpha_equiv ~msg:"uniquify: u2" ~printer:R1.Pp.pp;
  assert_equal u3' (uniquify u3) ~cmp:TestUtils.rlang_alpha_equiv ~msg:"uniquify: u3" ~printer:R1.Pp.pp;
  assert_equal u4' (uniquify u4) ~cmp:TestUtils.rlang_alpha_equiv ~msg:"uniquify: u4" ~printer:R1.Pp.pp

let test_is_uniquify _ctxt =
  assert_equal true (is_uniquify u1') ~msg:"is_uniquify: u1" ~printer:string_of_bool;
  assert_equal true (is_uniquify u2') ~msg:"is_uniquify: u2" ~printer:string_of_bool;
  assert_equal true (is_uniquify u3') ~msg:"is_uniquify: u3" ~printer:string_of_bool;
  assert_equal true (is_uniquify u4') ~msg:"is_uniquify: u4" ~printer:string_of_bool

let test_interp_uniquify _ctxt =
  assert_equal (interp ~inputs:[ 1 ] u1') (interp ~inputs:[ 1 ] u1) ~msg:"interp_uniquify: u1" ~printer:string_of_int;
  assert_equal (interp u2') (interp u2) ~msg:"interp_uniquify: u2" ~printer:string_of_int;
  assert_equal (interp u3') (interp u3) ~msg:"interp_uniquify: u3" ~printer:string_of_int;
  assert_equal (interp u4') (interp u4) ~msg:"interp_uniquify: u4" ~printer:string_of_int

let suite =
  "passes_tests"
  >::: [ "uniquify" >:: test_uniquify; "is_uniquify" >:: test_is_uniquify; "interp_uniquify" >:: test_interp_uniquify ]

let _ = run_test_tt_main suite
