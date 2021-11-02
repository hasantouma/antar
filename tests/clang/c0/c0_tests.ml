open OUnit2
open C0.Ast
open C0.Interp

let make_p (lst : (label * tail) list) : p = { info = false; blks = lst }

let c1 = make_p [ ("entry", Return (Number 5)) ]

let c2 = make_p [ ("entry", Seq (Set ("x", Arg (Number 42)), Return (Var "x"))) ]

let c3 = make_p [ ("entry", Seq (Set ("x", Arg (Number 42)), Seq (Set ("y", Arg (Var "x")), Return (Var "y")))) ]

let c4 = make_p [ ("entry", Seq (Set ("x", Read), Seq (Set ("y", Negate (Var "x")), Return (Var "y")))) ]

let c5 = make_p [ ("entry", Seq (Set ("x", Read), Seq (Set ("y", Add (Number (-5), Var "x")), Return (Var "y")))) ]

let c6 =
  make_p
    [ ( "entry"
      , Seq
          ( Set ("x", Read)
          , Seq (Set ("y", Add (Number (-5), Var "x")), Seq (Set ("z", Negate (Var "y")), Return (Var "z"))) ) )
    ]

let c7 =
  make_p
    [ ("foo", Seq (Set ("x", Arg (Number 42)), Return (Var "x")))
    ; ("entry", Seq (Set ("x", Arg (Number 12)), Return (Var "x")))
    ; ("bar", Seq (Set ("x", Arg (Number 52)), Return (Var "x")))
    ]

let c8 =
  make_p
    [ ( "entry"
      , Seq
          ( Set ("x", Read)
          , Seq (Set ("x", Add (Number (-5), Var "x")), Seq (Set ("z", Negate (Var "x")), Return (Var "z"))) ) )
    ]

let c9 =
  make_p
    [ ("entry", Seq (Set ("x", Read), Seq (Set ("y", Read), Seq (Set ("x", Add (Var "y", Var "x")), Return (Var "x")))))
    ]

let c10 =
  make_p
    [ ( "entry"
      , Seq (Set ("x", Read), Seq (Set ("y", Negate (Var "x")), Seq (Set ("z", Negate (Var "y")), Return (Var "z")))) )
    ]

let c11 =
  make_p
    [ ( "entry"
      , Seq
          ( Set ("x", Read)
          , Seq
              ( Set ("y", Read)
              , Seq (Set ("y", Negate (Var "y")), Seq (Set ("z", Add (Var "x", Var "y")), Return (Var "z"))) ) ) )
    ]

let c12 =
  make_p
    [ ( "entry"
      , Seq
          ( Set ("x", Read)
          , Seq
              ( Set ("y", Read)
              , Seq
                  ( Set ("w", Read)
                  , Seq (Set ("y", Negate (Var "y")), Seq (Set ("z", Add (Var "x", Var "y")), Return (Var "z"))) ) ) )
      )
    ]

let test_interp _ctxt =
  assert_equal 5 (interp c1) ~msg:"c1" ~printer:string_of_int;
  assert_equal 42 (interp c2) ~msg:"c2" ~printer:string_of_int;
  assert_equal 42 (interp c3) ~msg:"c3" ~printer:string_of_int;
  assert_equal (-10) (interp ~inputs:[ 10 ] c4) ~msg:"c4" ~printer:string_of_int;
  assert_equal (-17) (interp ~inputs:[ -12 ] c5) ~msg:"c5" ~printer:string_of_int;
  assert_equal 17 (interp ~inputs:[ -12 ] c6) ~msg:"c6" ~printer:string_of_int;
  assert_equal 12 (interp c7) ~msg:"c7" ~printer:string_of_int;
  assert_equal 17 (interp ~inputs:[ -12 ] c8) ~msg:"c8" ~printer:string_of_int;
  assert_equal (-7) (interp ~inputs:[ -9; 2 ] c9) ~msg:"c9" ~printer:string_of_int;
  assert_equal 5 (interp ~inputs:[ 5 ] c10) ~msg:"c10" ~printer:string_of_int;
  assert_equal (-1) (interp ~inputs:[ 2; 3 ] c11) ~msg:"c11" ~printer:string_of_int;
  assert_equal (-1) (interp ~inputs:[ 2; 3; 4 ] c12) ~msg:"c12" ~printer:string_of_int

let suite = "c0_tests" >::: [ "test_interp" >:: test_interp ]

let _ = run_test_tt_main suite
