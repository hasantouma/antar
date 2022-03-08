open OUnit2
open Xlang
open Passes

let pi1 : xprogram = wrap_x_entry [ Movq (Constant 5, Reg RAX) ]
let pi1' = pi1

let pi2 : xprogram =
  wrap_x_entry ~pinfo:[ "x0" ]
    [ Movq (Constant 6, Deref (RSP, -8)); Negq (Deref (RSP, -8)); Movq (Deref (RSP, -8), Reg RAX) ]

let pi2' = pi2

let pi3 : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Deref (RSP, -8))
    ; Addq (Constant 2, Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi3' : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -8))
    ; Addq (Constant 2, Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi4 : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "y"; "x"; "a" ]
    [ Movq (Constant 42, Deref (RSP, -32))
    ; Movq (Deref (RSP, -32), Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Deref (RSP, -8))
    ; Addq (Deref (RSP, -24), Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi4' : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "y"; "x"; "a" ]
    [ Movq (Constant 42, Deref (RSP, -32))
    ; Movq (Deref (RSP, -32), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -8))
    ; Movq (Deref (RSP, -24), Reg RAX)
    ; Addq (Reg RAX, Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi5 : xprogram =
  wrap_x_entry ~pinfo:[ "y"; "x" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Deref (RSP, -8))
    ; Negq (Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi5' : xprogram =
  wrap_x_entry ~pinfo:[ "y"; "x" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -8))
    ; Negq (Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi6 : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x"; "z"; "y" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -32))
    ; Movq (Constant 42, Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Movq (Deref (RSP, -24), Deref (RSP, -16))
    ; Addq (Deref (RSP, -32), Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Deref (RSP, -8))
    ; Negq (Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi6' : xprogram =
  wrap_x_entry ~pinfo:[ "x0"; "x"; "z"; "y" ]
    [ Callq "read_int"
    ; Movq (Reg RAX, Deref (RSP, -32))
    ; Movq (Constant 42, Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Movq (Deref (RSP, -24), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -32), Reg RAX)
    ; Addq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -8))
    ; Negq (Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi7 : xprogram =
  wrap_x_entry ~pinfo:[ "x1"; "x"; "y" ]
    [ Movq (Constant 10, Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Movq (Deref (RSP, -24), Deref (RSP, -16))
    ; Addq (Constant 42, Deref (RSP, -16))
    ; Movq (Constant 10, Deref (RSP, -8))
    ; Addq (Deref (RSP, -16), Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi7' : xprogram =
  wrap_x_entry ~pinfo:[ "x1"; "x"; "y" ]
    [ Movq (Constant 10, Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Movq (Deref (RSP, -24), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Addq (Constant 42, Deref (RSP, -16))
    ; Movq (Constant 10, Deref (RSP, -8))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Addq (Reg RAX, Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let test_patch_instructions ctxt =
  assert_equal pi1' (patch_instructions pi1) ~msg:"patch_instructions: pi1" ~printer:(Xlang.emitp false);
  assert_equal pi2' (patch_instructions pi2) ~msg:"patch_instructions: pi2" ~printer:(Xlang.emitp false);
  assert_equal pi3' (patch_instructions pi3) ~msg:"patch_instructions: pi3" ~printer:(Xlang.emitp false);
  assert_equal pi4' (patch_instructions pi4) ~msg:"patch_instructions: pi4" ~printer:(Xlang.emitp false);
  assert_equal pi5' (patch_instructions pi5) ~msg:"patch_instructions: pi5" ~printer:(Xlang.emitp false);
  assert_equal pi6' (patch_instructions pi6) ~msg:"patch_instructions: pi6" ~printer:(Xlang.emitp false);
  assert_equal pi7' (patch_instructions pi7) ~msg:"patch_instructions: pi7" ~printer:(Xlang.emitp false)

let test_is_patch_instructions ctxt =
  assert_equal true (is_patch_instructions pi1') ~msg:"is_patch_instructions: pi1" ~printer:string_of_bool;
  assert_equal true (is_patch_instructions pi2') ~msg:"is_patch_instructions: pi2" ~printer:string_of_bool;
  assert_equal true (is_patch_instructions pi3') ~msg:"is_patch_instructions: pi3" ~printer:string_of_bool;
  assert_equal true (is_patch_instructions pi4') ~msg:"is_patch_instructions: pi4" ~printer:string_of_bool;
  assert_equal true (is_patch_instructions pi5') ~msg:"is_patch_instructions: pi5" ~printer:string_of_bool;
  assert_equal true (is_patch_instructions pi6') ~msg:"is_patch_instructions: pi6" ~printer:string_of_bool;
  assert_equal true (is_patch_instructions pi7') ~msg:"is_patch_instructions: pi7" ~printer:string_of_bool

let test_interp_patch_instructions ctxt =
  assert_equal (interp pi1') (interp pi1) ~msg:"interp_patch_instructions: pi1" ~printer:string_of_int;
  assert_equal (interp pi2') (interp pi2) ~msg:"interp_patch_instructions: pi2" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 3 ] pi3')
    (interp ~inputs:[ 3 ] pi3)
    ~msg:"interp_patch_instructions: pi3" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 4 ] pi4')
    (interp ~inputs:[ 4 ] pi4)
    ~msg:"interp_patch_instructions: pi4" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 5 ] pi5')
    (interp ~inputs:[ 5 ] pi5)
    ~msg:"interp_patch_instructions: pi5" ~printer:string_of_int;
  assert_equal
    (interp ~inputs:[ 6 ] pi6')
    (interp ~inputs:[ 6 ] pi6)
    ~msg:"interp_patch_instructions: pi6" ~printer:string_of_int;
  assert_equal (interp pi7') (interp pi7) ~msg:"interp_patch_instructions: pi7" ~printer:string_of_int

let suite =
  "patch_instructions_tests"
  >::: [ "patch_instructions" >:: test_patch_instructions
       ; "is_patch_instructions" >:: test_is_patch_instructions
       ; "interp_patch_instructions" >:: test_interp_patch_instructions
       ]

let _ = run_test_tt_main suite
