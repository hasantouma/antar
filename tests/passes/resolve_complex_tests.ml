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

let rco5' = `ELet ("x0", `ELet ("x1", `ERead, `ENegate (`EVar "x1")), `EVar "x0")

let rco6 = `ENegate (`EAdd (`ERead, `ENegate (`EInt 42)))

let rco6' =
  `ELet ("x0", `ELet ("x1", `ERead, `ELet ("x2", `ENegate (`EInt 42), `EAdd (`EVar "x1", `EVar "x2"))), `EVar "x0")

let test_resolve_complex _ctxt =
  assert_equal rco1' (resolve_complex rco1) ~msg:"rco1" ~printer:R1.Pp.pp;
  assert_equal rco2' (resolve_complex rco2) ~msg:"rco2" ~printer:R1.Pp.pp;
  assert_equal rco3' (resolve_complex rco3) ~msg:"rco3" ~printer:R1.Pp.pp;
  assert_equal rco4' (resolve_complex rco4) ~msg:"rco4" ~printer:R1.Pp.pp;
  assert_equal rco5' (resolve_complex rco5) ~msg:"rco5" ~printer:R1.Pp.pp;
  assert_equal rco6' (resolve_complex rco6) ~msg:"rco6" ~printer:R1.Pp.pp

let suite = "resolve_complex_tests" >::: [ "resolve_complex" >:: test_resolve_complex ]

let _ = run_test_tt_main suite
