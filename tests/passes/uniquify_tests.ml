open OUnit2
open Passes.Uniquify

let u1 = `ELet ("a", `ERead, `EVar "a")

let u1' = `ELet ("x0", `ERead, `EVar "x0")

let u2 = `ELet ("a", `ELet ("a", `ENegate (`EInt 3), `EAdd (`EVar "a", `EVar "a")), `EVar "a")

let u2' = `ELet ("x0", `ELet ("x1", `ENegate (`EInt 3), `EAdd (`EVar "x1", `EVar "x1")), `EVar "x0")

let u3 = `ELet ("a", `EInt 4, `ELet ("a", `ENegate (`EInt 3), `EAdd (`EVar "a", `EVar "a")))

let u3' = `ELet ("x0", `EInt 4, `ELet ("x1", `ENegate (`EInt 3), `EAdd (`EVar "x1", `EVar "x1")))

let u4 =
  `EAdd
    ( `ELet ("x", `EInt 7, `EVar "x")
    , `ELet ("x", `EInt 8, `ELet ("x", `EAdd (`EInt 1, `EVar "x"), `EAdd (`EVar "x", `EVar "x"))) )

let u4' =
  `EAdd
    ( `ELet ("x0", `EInt 7, `EVar "x0")
    , `ELet ("x1", `EInt 8, `ELet ("x2", `EAdd (`EInt 1, `EVar "x1"), `EAdd (`EVar "x2", `EVar "x2"))) )

let test_uniquify _ctxt =
  assert_equal u1' (uniquify u1) ~cmp:TestUtils.alpha_equiv ~msg:"u1" ~printer:R1.Pp.pp;
  assert_equal u2' (uniquify u2) ~cmp:TestUtils.alpha_equiv ~msg:"u2" ~printer:R1.Pp.pp;
  assert_equal u3' (uniquify u3) ~cmp:TestUtils.alpha_equiv ~msg:"u3" ~printer:R1.Pp.pp;
  assert_equal u4' (uniquify u4) ~cmp:TestUtils.alpha_equiv ~msg:"u4" ~printer:R1.Pp.pp

let suite = "passes_tests" >::: [ "uniquify" >:: test_uniquify ]

let _ = run_test_tt_main suite
