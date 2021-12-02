open OUnit2
open C0.Ast
open Passes.Explicate_control

let make_cprog (tail : tail) : cprogram = { info = []; blks = [ ("entry", tail) ] }

let econ1 = `EInt 5

let econ1' = make_cprog (Return (Number 5))

let econ2 = `ENegate (`EInt 6)

let econ2' = make_cprog (Seq (Set ("x0", Negate (Number 6)), Return (Var "x0")))

let econ3 = `ELet ("x", `ERead, `EAdd (`EInt 2, `EVar "x"))

let econ3' = make_cprog (Seq (Set ("x", Read), Seq (Set ("x0", Add (Number 2, Var "x")), Return (Var "x0"))))

let econ4 = `ELet ("a", `EInt 42, `ELet ("x", `ENegate (`EVar "a"), `ELet ("y", `ERead, `EAdd (`EVar "x", `EVar "y"))))

let econ4' =
  make_cprog
    (Seq
       ( Set ("a", Arg (Number 42))
       , Seq
           ( Set ("x", Negate (Var "a"))
           , Seq (Set ("y", Read), Seq (Set ("x0", Add (Var "x", Var "y")), Return (Var "x0"))) ) ))

let econ5 = `ELet ("x", `ERead, `ENegate (`EVar "x"))

let econ5' = make_cprog (Seq (Set ("x", Read), Seq (Set ("y", Negate (Var "x")), Return (Var "y"))))

let econ6 =
  `ELet ("x", `ELet ("y", `ERead, `ELet ("z", `ENegate (`EInt 42), `EAdd (`EVar "y", `EVar "z"))), `ENegate (`EVar "x"))

let econ6' =
  make_cprog
    (Seq
       ( Set ("y", Read)
       , Seq
           ( Set ("z", Negate (Number 42))
           , Seq (Set ("x", Add (Var "y", Var "z")), Seq (Set ("x0", Negate (Var "x")), Return (Var "x0"))) ) ))

let econ7 = `ELet ("x", `ELet ("y", `ENegate (`EInt 10), `EAdd (`EInt 42, `EVar "y")), `EAdd (`EVar "x", `EInt 10))

let econ7' =
  make_cprog
    (Seq
       ( Set ("y", Negate (Number 10))
       , Seq (Set ("x", Add (Number 42, Var "y")), Seq (Set ("x1", Add (Var "x", Number 10)), Return (Var "x1"))) ))

let test_explicate_control _ctxt =
  assert_equal econ1' (explicate_control econ1) ~cmp:TestUtils.clang_alpha_equiv ~msg:"econ1" ~printer:C0.Pp.pp;
  assert_equal econ2' (explicate_control econ2) ~cmp:TestUtils.clang_alpha_equiv ~msg:"econ2" ~printer:C0.Pp.pp;
  assert_equal econ3' (explicate_control econ3) ~cmp:TestUtils.clang_alpha_equiv ~msg:"econ3" ~printer:C0.Pp.pp;
  assert_equal econ4' (explicate_control econ4) ~cmp:TestUtils.clang_alpha_equiv ~msg:"econ4" ~printer:C0.Pp.pp;
  assert_equal econ5' (explicate_control econ5) ~cmp:TestUtils.clang_alpha_equiv ~msg:"econ5" ~printer:C0.Pp.pp;
  assert_equal econ6' (explicate_control econ6) ~cmp:TestUtils.clang_alpha_equiv ~msg:"econ6" ~printer:C0.Pp.pp;
  assert_equal econ7' (explicate_control econ7) ~cmp:TestUtils.clang_alpha_equiv ~msg:"econ7" ~printer:C0.Pp.pp

let test_is_resolve_complex _ctxt =
  assert_equal true
    (Passes.Resolve_complex.is_resolve_complex econ1)
    ~msg:"is_resolve_complex: econ1" ~printer:string_of_bool;
  assert_equal true
    (Passes.Resolve_complex.is_resolve_complex econ2)
    ~msg:"is_resolve_complex: econ2" ~printer:string_of_bool;
  assert_equal true
    (Passes.Resolve_complex.is_resolve_complex econ3)
    ~msg:"is_resolve_complex: econ3" ~printer:string_of_bool;
  assert_equal true
    (Passes.Resolve_complex.is_resolve_complex econ4)
    ~msg:"is_resolve_complex: econ4" ~printer:string_of_bool;
  assert_equal true
    (Passes.Resolve_complex.is_resolve_complex econ5)
    ~msg:"is_resolve_complex: econ5" ~printer:string_of_bool;
  assert_equal true
    (Passes.Resolve_complex.is_resolve_complex econ6)
    ~msg:"is_resolve_complex: econ6" ~printer:string_of_bool;
  assert_equal true
    (Passes.Resolve_complex.is_resolve_complex econ7)
    ~msg:"is_resolve_complex: econ7" ~printer:string_of_bool

let test_interp_explicate_control _ctxt =
  assert_equal (C0.Interp.interp econ1') (R1.Interp.interp econ1) ~msg:"interp_explicate_control: econ1"
    ~printer:string_of_int;
  assert_equal (C0.Interp.interp econ2') (R1.Interp.interp econ2) ~msg:"interp_explicate_control: econ2"
    ~printer:string_of_int;
  assert_equal
    (C0.Interp.interp ~inputs:[ 42 ] econ3')
    (R1.Interp.interp ~inputs:[ 42 ] econ3)
    ~msg:"interp_explicate_control: econ3" ~printer:string_of_int;
  assert_equal
    (C0.Interp.interp ~inputs:[ 42 ] econ4')
    (R1.Interp.interp ~inputs:[ 42 ] econ4)
    ~msg:"interp_explicate_control: econ4" ~printer:string_of_int;
  assert_equal
    (C0.Interp.interp ~inputs:[ 42 ] econ5')
    (R1.Interp.interp ~inputs:[ 42 ] econ5)
    ~msg:"interp_explicate_control: econ5" ~printer:string_of_int;
  assert_equal
    (C0.Interp.interp ~inputs:[ 42 ] econ6')
    (R1.Interp.interp ~inputs:[ 42 ] econ6)
    ~msg:"interp_explicate_control: econ6" ~printer:string_of_int;
  assert_equal (C0.Interp.interp econ7') (R1.Interp.interp econ7) ~msg:"interp_explicate_control: econ7"
    ~printer:string_of_int

let suite =
  "explicate_control_tests"
  >::: [ "explicate_control" >:: test_explicate_control
       ; "is_resolve_complex" >:: test_is_resolve_complex
       ; "interp_explicate_control" >:: test_interp_explicate_control
       ]

let _ = run_test_tt_main suite
