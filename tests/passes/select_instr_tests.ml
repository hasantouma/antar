open OUnit2
open Xlang
open Clang
open Passes

let ul1 = wrap_c_entry (Return (Number 5))
let ul1' = { ul1 with info = [] }
let si1 : xprogram = wrap_x_entry [ Movq (Constant 5, Reg RAX) ]
let ul2 = wrap_c_entry (Seq (Set ("x0", Negate (Number 6)), Return (Var "x0")))
let ul2' = { ul2 with info = [ "x0" ] }

let si2 : xprogram =
  wrap_x_entry ~pinfo:ul2'.info [ Movq (Constant 6, Ref "x0"); Negq (Ref "x0"); Movq (Ref "x0", Reg RAX) ]

let ul3 = wrap_c_entry (Seq (Set ("x", Read), Seq (Set ("x0", Add (Number 2, Var "x")), Return (Var "x0"))))
let ul3' = { ul3 with info = [ "x0"; "x" ] }

let si3 : xprogram =
  wrap_x_entry ~pinfo:ul3'.info
    [ Callq "read_int"
    ; Movq (Reg RAX, Ref "x")
    ; Movq (Ref "x", Ref "x0")
    ; Addq (Constant 2, Ref "x0")
    ; Movq (Ref "x0", Reg RAX)
    ]

let ul4 =
  wrap_c_entry
    (Seq
       ( Set ("a", Arg (Number 42))
       , Seq
           ( Set ("x", Negate (Var "a"))
           , Seq (Set ("y", Read), Seq (Set ("x0", Add (Var "x", Var "y")), Return (Var "x0"))) ) ))

let ul4' = { ul4 with info = [ "x0"; "y"; "x"; "a" ] }

let si4 : xprogram =
  wrap_x_entry ~pinfo:ul4'.info
    [ Movq (Constant 42, Ref "a")
    ; Movq (Ref "a", Ref "x")
    ; Negq (Ref "x")
    ; Callq "read_int"
    ; Movq (Reg RAX, Ref "y")
    ; Movq (Ref "y", Ref "x0")
    ; Addq (Ref "x", Ref "x0")
    ; Movq (Ref "x0", Reg RAX)
    ]

let ul5 = wrap_c_entry (Seq (Set ("x", Read), Seq (Set ("y", Negate (Var "x")), Return (Var "y"))))
let ul5' = { ul5 with info = [ "y"; "x" ] }

let si5 : xprogram =
  wrap_x_entry ~pinfo:ul5'.info
    [ Callq "read_int"; Movq (Reg RAX, Ref "x"); Movq (Ref "x", Ref "y"); Negq (Ref "y"); Movq (Ref "y", Reg RAX) ]

let ul6 =
  wrap_c_entry
    (Seq
       ( Set ("y", Read)
       , Seq
           ( Set ("z", Negate (Number 42))
           , Seq (Set ("x", Add (Var "y", Var "z")), Seq (Set ("x0", Negate (Var "x")), Return (Var "x0"))) ) ))

let ul6' = { ul6 with info = [ "x0"; "x"; "z"; "y" ] }

let si6 : xprogram =
  wrap_x_entry ~pinfo:ul6'.info
    [ Callq "read_int"
    ; Movq (Reg RAX, Ref "y")
    ; Movq (Constant 42, Ref "z")
    ; Negq (Ref "z")
    ; Movq (Ref "z", Ref "x")
    ; Addq (Ref "y", Ref "x")
    ; Movq (Ref "x", Ref "x0")
    ; Negq (Ref "x0")
    ; Movq (Ref "x0", Reg RAX)
    ]

let ul7 =
  wrap_c_entry
    (Seq
       ( Set ("y", Negate (Number 10))
       , Seq (Set ("x", Add (Number 42, Var "y")), Seq (Set ("x1", Add (Var "x", Number 10)), Return (Var "x1"))) ))

let ul7' = { ul7 with info = [ "x1"; "x"; "y" ] }

let si7 : xprogram =
  wrap_x_entry ~pinfo:ul7'.info
    [ Movq (Constant 10, Ref "y")
    ; Negq (Ref "y")
    ; Movq (Ref "y", Ref "x")
    ; Addq (Constant 42, Ref "x")
    ; Movq (Constant 10, Ref "x1")
    ; Addq (Ref "x", Ref "x1")
    ; Movq (Ref "x1", Reg RAX)
    ]

(* Testing helper function. 'uncover_locals' gets called as a part of 'select_instr' *)
let test_uncover_locals ctxt =
  assert_equal ul1' (uncover_locals ul1) ~msg:"uncover_locals: ul1" ~printer:Clang.pp;
  assert_equal ul2' (uncover_locals ul2) ~msg:"uncover_locals: ul2" ~printer:Clang.pp;
  assert_equal ul3' (uncover_locals ul3) ~msg:"uncover_locals: ul3" ~printer:Clang.pp;
  assert_equal ul4' (uncover_locals ul4) ~msg:"uncover_locals: ul4" ~printer:Clang.pp;
  assert_equal ul5' (uncover_locals ul5) ~msg:"uncover_locals: ul5" ~printer:Clang.pp;
  assert_equal ul6' (uncover_locals ul6) ~msg:"uncover_locals: ul6" ~printer:Clang.pp;
  assert_equal ul7' (uncover_locals ul7) ~msg:"uncover_locals: ul7" ~printer:Clang.pp

let test_select_instr ctxt =
  assert_equal si1 (select_instr ul1) ~msg:"select_instr: si1" ~printer:(Xlang.emitp true);
  assert_equal si2 (select_instr ul2) ~msg:"select_instr: si2" ~printer:(Xlang.emitp true);
  assert_equal si3 (select_instr ul3) ~msg:"select_instr: si3" ~printer:(Xlang.emitp true);
  assert_equal si4 (select_instr ul4) ~msg:"select_instr: si4" ~printer:(Xlang.emitp true);
  assert_equal si5 (select_instr ul5) ~msg:"select_instr: si5" ~printer:(Xlang.emitp true);
  assert_equal si6 (select_instr ul6) ~msg:"select_instr: si6" ~printer:(Xlang.emitp true);
  assert_equal si7 (select_instr ul7) ~msg:"select_instr: si7" ~printer:(Xlang.emitp true)

let test_interp_select_instr ctxt =
  assert_equal (Xlang.interp si1) (Clang.interp ul1') ~msg:"interp_select_instr: si1" ~printer:string_of_int;
  assert_equal (Xlang.interp si2) (Clang.interp ul2') ~msg:"interp_select_instr: si2" ~printer:string_of_int;
  assert_equal (Xlang.interp ~inputs:[ 3 ] si3) (Clang.interp ~inputs:[ 3 ] ul3') ~msg:"interp_select_instr: si3"
    ~printer:string_of_int;
  assert_equal (Xlang.interp ~inputs:[ 4 ] si4) (Clang.interp ~inputs:[ 4 ] ul4') ~msg:"interp_select_instr: si4"
    ~printer:string_of_int;
  assert_equal (Xlang.interp ~inputs:[ 5 ] si5) (Clang.interp ~inputs:[ 5 ] ul5') ~msg:"interp_select_instr: si5"
    ~printer:string_of_int;
  assert_equal (Xlang.interp ~inputs:[ 6 ] si6) (Clang.interp ~inputs:[ 6 ] ul6') ~msg:"interp_select_instr: si6"
    ~printer:string_of_int;
  assert_equal (Xlang.interp si7) (Clang.interp ul7') ~msg:"interp_select_instr: si7" ~printer:string_of_int

let suite =
  "select_instr_tests"
  >::: [ "uncover_locals" >:: test_uncover_locals
       ; "select_instr" >:: test_select_instr
       ; "interp_select_instr" >:: test_interp_select_instr
       ]

let _ = run_test_tt_main suite
