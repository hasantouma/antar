open OUnit2
open X0.Ast
open Passes.Patch_instructions

let make_xprog ?(pinfo = []) (lst : (label * instr list) list) : xprogram =
  let blocks =
    List.map
      (fun (label, instrs) ->
        let block = { info = []; instrs } in
        (label, block))
      lst
  in
  { info = pinfo; blks = blocks }

let stack_space (lst : C0.Ast.var list) : int =
  let n = List.length lst in
  if n mod 2 = 0 then
    8 * n
  else
    8 * (n + 1)

let wrap (vars_length : int) (lst : instr list) : instr list =
  let prologue = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP); Subq (Constant vars_length, Reg RSP) ] in
  let epilogue = [ Addq (Constant vars_length, Reg RSP); Popq (Reg RBP); Retq ] in
  List.append (List.append prologue lst) epilogue

let wrap_entry ?(pinfo = []) (instrs : instr list) : xprogram =
  let vars_length = stack_space pinfo in
  let entry = wrap vars_length instrs in
  make_xprog ~pinfo [ ("entry", entry) ]

let pi1 : xprogram = wrap_entry [ Movq (Constant 5, Reg RAX) ]

let pi1' = pi1

let pi2 : xprogram =
  wrap_entry ~pinfo:[ "x0" ]
    [ Movq (Constant 6, Deref (RSP, -8)); Negq (Deref (RSP, -8)); Movq (Deref (RSP, -8), Reg RAX) ]

let pi2' = pi2

let pi3 : xprogram =
  wrap_entry ~pinfo:[ "x0"; "x" ]
    [ Callq "read"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Deref (RSP, -8))
    ; Addq (Constant 2, Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi3' : xprogram =
  wrap_entry ~pinfo:[ "x0"; "x" ]
    [ Callq "read"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -8))
    ; Addq (Constant 2, Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi4 : xprogram =
  wrap_entry ~pinfo:[ "x0"; "y"; "x"; "a" ]
    [ Movq (Constant 42, Deref (RSP, -32))
    ; Movq (Deref (RSP, -32), Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Callq "read"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Deref (RSP, -8))
    ; Addq (Deref (RSP, -24), Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi4' : xprogram =
  wrap_entry ~pinfo:[ "x0"; "y"; "x"; "a" ]
    [ Movq (Constant 42, Deref (RSP, -32))
    ; Movq (Deref (RSP, -32), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Callq "read"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -8))
    ; Movq (Deref (RSP, -24), Reg RAX)
    ; Addq (Reg RAX, Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi5 : xprogram =
  wrap_entry ~pinfo:[ "y"; "x" ]
    [ Callq "read"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Deref (RSP, -8))
    ; Negq (Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi5' : xprogram =
  wrap_entry ~pinfo:[ "y"; "x" ]
    [ Callq "read"
    ; Movq (Reg RAX, Deref (RSP, -16))
    ; Movq (Deref (RSP, -16), Reg RAX)
    ; Movq (Reg RAX, Deref (RSP, -8))
    ; Negq (Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi6 : xprogram =
  wrap_entry ~pinfo:[ "x0"; "x"; "z"; "y" ]
    [ Callq "read"
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
  wrap_entry ~pinfo:[ "x0"; "x"; "z"; "y" ]
    [ Callq "read"
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
  wrap_entry ~pinfo:[ "x1"; "x"; "y" ]
    [ Movq (Constant 10, Deref (RSP, -24))
    ; Negq (Deref (RSP, -24))
    ; Movq (Deref (RSP, -24), Deref (RSP, -16))
    ; Addq (Constant 42, Deref (RSP, -16))
    ; Movq (Constant 10, Deref (RSP, -8))
    ; Addq (Deref (RSP, -16), Deref (RSP, -8))
    ; Movq (Deref (RSP, -8), Reg RAX)
    ]

let pi7' : xprogram =
  wrap_entry ~pinfo:[ "x1"; "x"; "y" ]
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

let test_patch_instructions _ctxt =
  assert_equal pi1' (patch_instructions pi1) ~msg:"patch_instructions: pi1" ~printer:(X0.Emit.emitp false);
  assert_equal pi2' (patch_instructions pi2) ~msg:"patch_instructions: pi2" ~printer:(X0.Emit.emitp false);
  assert_equal pi3' (patch_instructions pi3) ~msg:"patch_instructions: pi3" ~printer:(X0.Emit.emitp false);
  assert_equal pi4' (patch_instructions pi4) ~msg:"patch_instructions: pi4" ~printer:(X0.Emit.emitp false);
  assert_equal pi5' (patch_instructions pi5) ~msg:"patch_instructions: pi5" ~printer:(X0.Emit.emitp false);
  assert_equal pi6' (patch_instructions pi6) ~msg:"patch_instructions: pi6" ~printer:(X0.Emit.emitp false);
  assert_equal pi7' (patch_instructions pi7) ~msg:"patch_instructions: pi7" ~printer:(X0.Emit.emitp false)

let suite = "patch_instructions_tests" >::: [ "patch_instructions" >:: test_patch_instructions ]

let _ = run_test_tt_main suite