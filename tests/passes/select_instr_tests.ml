open OUnit2
open C0.Ast
open Passes.Select_instr

let make_p (tail : tail) : p = { info = []; blks = [ ("entry", tail) ] }

let ul1 = make_p (Return (Number 5))

let ul1' = { ul1 with info = [] }

let ul2 = make_p (Seq (Set ("x0", Negate (Number 6)), Return (Var "x0")))

let ul2' = { ul2 with info = [ "x0" ] }

let ul3 = make_p (Seq (Set ("x", Read), Seq (Set ("x0", Add (Number 2, Var "x")), Return (Var "x0"))))

let ul3' = { ul3 with info = [ "x0"; "x" ] }

let ul4 =
  make_p
    (Seq
       (Set ("x", Negate (Var "a")), Seq (Set ("y", Read), Seq (Set ("x0", Add (Var "x", Var "y")), Return (Var "x0")))))

let ul4' = { ul4 with info = [ "x0"; "y"; "x" ] }

let ul5 = make_p (Seq (Set ("x", Read), Seq (Set ("y", Negate (Var "x")), Return (Var "y"))))

let ul5' = { ul5 with info = [ "y"; "x" ] }

let ul6 =
  make_p
    (Seq
       ( Set ("y", Read)
       , Seq
           ( Set ("z", Negate (Number 42))
           , Seq (Set ("x", Add (Var "y", Var "z")), Seq (Set ("x0", Negate (Var "x")), Return (Var "x0"))) ) ))

let ul6' = { ul6 with info = [ "x0"; "x"; "z"; "y" ] }

let ul7 =
  make_p
    (Seq
       ( Set ("y", Negate (Number 10))
       , Seq (Set ("x", Add (Number 42, Var "y")), Seq (Set ("x1", Add (Var "x", Number 10)), Return (Var "x1"))) ))

let ul7' = { ul7 with info = [ "x1"; "x"; "y" ] }

let test_uncover_locals _ctxt =
  assert_equal ul1' (uncover_locals ul1) ~msg:"uncover_locals: ul1" ~printer:C0.Pp.pp;
  assert_equal ul2' (uncover_locals ul2) ~msg:"uncover_locals: ul2" ~printer:C0.Pp.pp;
  assert_equal ul3' (uncover_locals ul3) ~msg:"uncover_locals: ul3" ~printer:C0.Pp.pp;
  assert_equal ul4' (uncover_locals ul4) ~msg:"uncover_locals: ul4" ~printer:C0.Pp.pp;
  assert_equal ul5' (uncover_locals ul5) ~msg:"uncover_locals: ul5" ~printer:C0.Pp.pp;
  assert_equal ul6' (uncover_locals ul6) ~msg:"uncover_locals: ul6" ~printer:C0.Pp.pp;
  assert_equal ul7' (uncover_locals ul7) ~msg:"uncover_locals: ul7" ~printer:C0.Pp.pp

let suite = "select_instr_tests" >::: [ "uncover_locals" >:: test_uncover_locals ]

let _ = run_test_tt_main suite
