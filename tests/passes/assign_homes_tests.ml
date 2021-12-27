open OUnit2
open Xlang
open Passes

let ah1 : xprogram = wrap_x_entry [ Movq (Constant 5, Reg RAX) ]
let ah1' = ah1

let ah2 : xprogram =
  wrap_x_entry ~pinfo:[ "x0" ] [ Movq (Constant 6, Ref "x0"); Negq (Ref "x0"); Movq (Ref "x0", Reg RAX) ]

let ah2' : xprogram =
  wrap_x_entry ~pinfo:[ "x0" ]
    [ Movq (Constant 6, Deref (RBP, -8)); Negq (Deref (RBP, -8)); Movq (Deref (RBP, -8), Reg RAX) ]

let ah3 : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Ref "x")
    ; Movq (Ref "x", Ref "x0")
    ; Addq (Constant 2, Ref "x0")
    ; Movq (Ref "x0", Reg RAX)
    ]

let ah3' : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RBP, -16))
    ; Movq (Deref (RBP, -16), Deref (RBP, -8))
    ; Addq (Constant 2, Deref (RBP, -8))
    ; Movq (Deref (RBP, -8), Reg RAX)
    ]

let ah4 : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "y"; "x"; "a" ]
    [ Movq (Constant 42, Ref "a")
    ; Movq (Ref "a", Ref "x")
    ; Negq (Ref "x")
    ; Callq "read_int"
    ; Movq (Reg RAX, Ref "y")
    ; Movq (Ref "y", Ref "x0")
    ; Addq (Ref "x", Ref "x0")
    ; Movq (Ref "x0", Reg RAX)
    ]

let ah4' : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "y"; "x"; "a" ]
    [ Movq (Constant 42, Deref (RBP, -32))
    ; Movq (Deref (RBP, -32), Deref (RBP, -24))
    ; Negq (Deref (RBP, -24))
    ; Callq "read_int"
    ; Movq (Reg RAX, Deref (RBP, -16))
    ; Movq (Deref (RBP, -16), Deref (RBP, -8))
    ; Addq (Deref (RBP, -24), Deref (RBP, -8))
    ; Movq (Deref (RBP, -8), Reg RAX)
    ]

let ah5 : xprogram =
  wrap_x_entry ~pinfo:[ "y"; "x" ]
    [ Callq "read_int"; Movq (Reg RAX, Ref "x"); Movq (Ref "x", Ref "y"); Negq (Ref "y"); Movq (Ref "y", Reg RAX) ]

let ah5' : xprogram =
  wrap_x_entry ~pinfo:[ "y"; "x" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RBP, -16))
    ; Movq (Deref (RBP, -16), Deref (RBP, -8))
    ; Negq (Deref (RBP, -8))
    ; Movq (Deref (RBP, -8), Reg RAX)
    ]

let ah6 : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x"; "z"; "y" ]
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

let ah6' : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x"; "z"; "y" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RBP, -32))
    ; Movq (Constant 42, Deref (RBP, -24))
    ; Negq (Deref (RBP, -24))
    ; Movq (Deref (RBP, -24), Deref (RBP, -16))
    ; Addq (Deref (RBP, -32), Deref (RBP, -16))
    ; Movq (Deref (RBP, -16), Deref (RBP, -8))
    ; Negq (Deref (RBP, -8))
    ; Movq (Deref (RBP, -8), Reg RAX)
    ]

let ah7 : xprogram =
  wrap_x_entry ~pinfo:[ "x1"; "x"; "y" ]
    [ Movq (Constant 10, Ref "y")
    ; Negq (Ref "y")
    ; Movq (Ref "y", Ref "x")
    ; Addq (Constant 42, Ref "x")
    ; Movq (Constant 10, Ref "x1")
    ; Addq (Ref "x", Ref "x1")
    ; Movq (Ref "x1", Reg RAX)
    ]

let ah7' : xprogram =
  wrap_x_entry ~pinfo:[ "x1"; "x"; "y" ]
    [ Movq (Constant 10, Deref (RBP, -24))
    ; Negq (Deref (RBP, -24))
    ; Movq (Deref (RBP, -24), Deref (RBP, -16))
    ; Addq (Constant 42, Deref (RBP, -16))
    ; Movq (Constant 10, Deref (RBP, -8))
    ; Addq (Deref (RBP, -16), Deref (RBP, -8))
    ; Movq (Deref (RBP, -8), Reg RAX)
    ]

let test_assign_homes _ctxt =
  assert_equal ah1' (assign_homes ah1) ~msg:"assign_homes: ah1" ~printer:(Xlang.emitp true);
  assert_equal ah2' (assign_homes ah2) ~msg:"assign_homes: ah2" ~printer:(Xlang.emitp true);
  assert_equal ah3' (assign_homes ah3) ~msg:"assign_homes: ah3" ~printer:(Xlang.emitp true);
  assert_equal ah4' (assign_homes ah4) ~msg:"assign_homes: ah4" ~printer:(Xlang.emitp true);
  assert_equal ah5' (assign_homes ah5) ~msg:"assign_homes: ah5" ~printer:(Xlang.emitp true);
  assert_equal ah6' (assign_homes ah6) ~msg:"assign_homes: ah6" ~printer:(Xlang.emitp true);
  assert_equal ah7' (assign_homes ah7) ~msg:"assign_homes: ah7" ~printer:(Xlang.emitp true)

let test_is_assign_homes _ctxt =
  assert_equal true (is_assign_homes ah1') ~msg:"is_assign_homes: ah1" ~printer:string_of_bool;
  assert_equal true (is_assign_homes ah2') ~msg:"is_assign_homes: ah2" ~printer:string_of_bool;
  assert_equal true (is_assign_homes ah3') ~msg:"is_assign_homes: ah3" ~printer:string_of_bool;
  assert_equal true (is_assign_homes ah4') ~msg:"is_assign_homes: ah4" ~printer:string_of_bool;
  assert_equal true (is_assign_homes ah5') ~msg:"is_assign_homes: ah5" ~printer:string_of_bool;
  assert_equal true (is_assign_homes ah6') ~msg:"is_assign_homes: ah6" ~printer:string_of_bool;
  assert_equal true (is_assign_homes ah7') ~msg:"is_assign_homes: ah7" ~printer:string_of_bool

let test_interp_assign_homes _ctxt =
  assert_equal (interp ah1') (interp ah1) ~msg:"interp_assign_homes: ah1" ~printer:string_of_int;
  assert_equal (interp ah2') (interp ah2) ~msg:"interp_assign_homes: ah2" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 3 ] ah3')
    (interp ~inputs:[ 3 ] ah3)
    ~msg:"interp_assign_homes: ah3" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 4 ] ah4')
    (interp ~inputs:[ 4 ] ah4)
    ~msg:"interp_assign_homes: ah4" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 5 ] ah5')
    (interp ~inputs:[ 5 ] ah5)
    ~msg:"interp_assign_homes: ah5" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 6 ] ah6')
    (interp ~inputs:[ 6 ] ah6)
    ~msg:"interp_assign_homes: ah6" ~printer:string_of_int;
  assert_equal (interp ah7') (interp ah7) ~msg:"interp_assign_homes: ah7" ~printer:string_of_int

let suite =
  "assign_homes_tests"
  >::: [ "assign_homes" >:: test_assign_homes
       ; "is_assign_homes" >:: test_is_assign_homes
       ; "interp_assign_homes" >:: test_interp_assign_homes
       ]

let _ = run_test_tt_main suite
