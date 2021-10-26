open OUnit2
open Passes.Resolve_complex

let rco1 = `EInt 5

let rco1' = `EInt 5

let rco2 = `ENegate (`EInt 6)

let rco2' = `ENegate (`EInt 6)

let rco3 = `EAdd (`EInt 2, `ERead)

let rco3' = `ELet ("x0", `ERead, `EAdd (`EInt 2, `EVar "x0"))

let rco4 = `EAdd (`ENegate (`EVar "a"), `ERead)

let rco4' = `ELet ("x0", `ENegate (`EVar "a"), `ELet ("x1", `ERead, `EAdd (`EVar "x0", `EVar "x1")))

let rco5 = `ENegate `ERead

let rco5' = `ELet ("x1", `ERead, `ENegate (`EVar "x1"))

let rco6 = `ENegate (`EAdd (`ERead, `ENegate (`EInt 42)))

let rco6' =
  `ELet
    ( "x0"
    , `ELet ("x1", `ERead, `ELet ("x2", `ENegate (`EInt 42), `EAdd (`EVar "x1", `EVar "x2")))
    , `ENegate (`EVar "x0") )

let rco7 = `ELet ("x", `EAdd (`EInt 42, `ENegate (`EInt 10)), `EAdd (`EVar "x", `EInt 10))

let rco7' = `ELet ("x", `ELet ("x0", `ENegate (`EInt 10), `EAdd (`EInt 42, `EVar "x0")), `EAdd (`EVar "x", `EInt 10))

let rco8 = `ELet ("y", `ELet ("x0", `EInt 20, `EAdd (`EVar "x0", `ELet ("x1", `EInt 22, `EVar "x1"))), `EVar "y")

let rco8' = `ELet ("y", `ELet ("x0", `EInt 20, `ELet ("x1", `EInt 22, `EAdd (`EVar "x0", `EVar "x1"))), `EVar "y")

let test_resolve_complex _ctxt =
  assert_equal rco1' (resolve_complex rco1) ~cmp:TestUtils.alpha_equiv ~msg:"rco1" ~printer:R1.Pp.pp;
  assert_equal rco2' (resolve_complex rco2) ~cmp:TestUtils.alpha_equiv ~msg:"rco2" ~printer:R1.Pp.pp;
  assert_equal rco3' (resolve_complex rco3) ~cmp:TestUtils.alpha_equiv ~msg:"rco3" ~printer:R1.Pp.pp;
  assert_equal rco4' (resolve_complex rco4) ~cmp:TestUtils.alpha_equiv ~msg:"rco4" ~printer:R1.Pp.pp;
  assert_equal rco5' (resolve_complex rco5) ~cmp:TestUtils.alpha_equiv ~msg:"rco5" ~printer:R1.Pp.pp;
  assert_equal rco6' (resolve_complex rco6) ~cmp:TestUtils.alpha_equiv ~msg:"rco6" ~printer:R1.Pp.pp;
  assert_equal rco7' (resolve_complex rco7) ~cmp:TestUtils.alpha_equiv ~msg:"rco7" ~printer:R1.Pp.pp;
  assert_equal rco8' (resolve_complex rco8) ~cmp:TestUtils.alpha_equiv
    ~msg:("rco8\n" ^ R1.Pp.pp rco8 ^ "\n")
    ~printer:(fun x -> "\n" ^ R1.Pp.pp x ^ "\n")

let suite = "resolve_complex_tests" >::: [ "resolve_complex" >:: test_resolve_complex ]

let _ = run_test_tt_main suite
