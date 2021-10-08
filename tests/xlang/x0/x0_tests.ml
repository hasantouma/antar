open OUnit2
open X0.Ast
open X0.Interp

let make_p (lst : (label * instr list) list) : p =
  let blocks =
    List.map
      (fun (label, instrs) ->
        let block = { info = false; instrs } in
        (label, block))
      lst
  in
  { info = false; blks = blocks }

let wrap (lst : instr list) : instr list =
  let prologue = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP) ] in
  let epilogue = [ Popq (Reg RBP); Retq ] in
  List.append (List.append prologue lst) epilogue

let wrap_entry (instrs : instr list) : p =
  let entry = wrap instrs in
  make_p [ ("_entry", entry) ]

let s1 : p = wrap_entry [ Movq (Constant 42, Reg RAX) ]

let s2 : p = wrap_entry [ Movq (Constant 12, Reg RAX); Addq (Constant 2, Reg RAX) ]

let s3 : p = wrap_entry [ Movq (Constant 5, Reg RAX); Subq (Constant 10, Reg RAX) ]

let s4 : p = wrap_entry [ Movq (Constant 32, Reg RAX); Movq (Constant 1, Reg RBX); Subq (Reg RBX, Reg RAX) ]

let s5 : p = wrap_entry [ Movq (Constant (-57), Reg RAX); Negq (Reg RAX) ]

let s6 : p =
  wrap_entry
    [ Movq (Constant 23, Reg RAX)
    ; Pushq (Reg RAX)
    ; Movq (Constant 8, Reg RBX)
    ; Pushq (Reg RBX)
    ; Popq (Reg RAX)
    ; Popq (Reg RBX)
    ; Subq (Reg RBX, Reg RAX)
    ]

let s7 : p =
  let entry = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP); Movq (Constant 42, Reg RAX); Jmp "foo" ] in
  let foo = [ Addq (Constant 1, Reg RAX); Popq (Reg RBP); Retq ] in
  make_p [ ("_entry", entry); ("foo", foo) ]

let s8 : p =
  let entry = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP); Pushq (Constant 17); Jmp "foo" ] in
  let foo = [ Popq (Reg RBX); Jmp "bar" ] in
  let bar = [ Movq (Reg RBX, Reg RAX); Popq (Reg RBP); Retq ] in
  make_p [ ("_entry", entry); ("foo", foo); ("bar", bar) ]

let s9 : p = wrap_entry [ Pushq (Constant 42); Movq (Deref (RSP, 0), Reg RAX); Addq (Constant 8, Reg RSP) ]

let s10 : p =
  let start =
    [ Movq (Constant 10, Deref (RBP, -8))
    ; Negq (Deref (RBP, -8))
    ; Movq (Deref (RBP, -8), Reg RAX)
    ; Addq (Constant 52, Reg RAX)
    ; Jmp "finish"
    ]
  in
  let entry = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP); Subq (Constant 16, Reg RSP); Jmp "start" ] in
  let finish = [ Addq (Constant 16, Reg RSP); Popq (Reg RBP); Retq ] in
  make_p [ ("start", start); ("_entry", entry); ("finish", finish) ]

let s11 : p =
  let entry = wrap [ Movq (Constant 13, Ref "hi"); Callq "foo" ] in
  let foo = wrap [ Addq (Constant 10, Ref "hi"); Movq (Ref "hi", Reg RAX) ] in
  make_p [ ("_entry", entry); ("foo", foo) ]

let s12 : p = wrap_entry [ Callq "read" ]

let s13 : p = wrap_entry [ Movq (Constant 5, Reg RAX); Movq (Constant 4, Reg RAX) ]

let s14 : p = wrap_entry [ Movq (Constant 5, Reg RAX); Addq (Reg RAX, Reg RAX) ]

let s15 : p =
  wrap_entry
    [ Movq (Constant 5, Reg RAX)
    ; Pushq (Reg RAX)
    ; Popq (Reg RBX)
    ; Movq (Constant 6, Reg RAX)
    ; Pushq (Reg RAX)
    ; Popq (Reg RCX)
    ; Addq (Reg RBX, Reg RCX)
    ; Subq (Reg RCX, Reg RAX)
    ]

let s16 : p = wrap_entry [ Movq (Constant 42, Reg RAX); Retq; Movq (Constant 15, Reg RAX) ]

let test _ctxt =
  assert_equal 42 (interp s1) ~msg:"Movq" ~printer:string_of_int;
  assert_equal 14 (interp s2) ~msg:"Addq" ~printer:string_of_int;
  assert_equal (-5) (interp s3) ~msg:"Subq" ~printer:string_of_int;
  assert_equal 31 (interp s4) ~msg:"Subq Regs" ~printer:string_of_int;
  assert_equal 57 (interp s5) ~msg:"Negq" ~printer:string_of_int;
  assert_equal (-15) (interp s6) ~msg:"Pushq and Popq" ~printer:string_of_int;
  assert_equal 43 (interp s7) ~msg:"Jmp to label" ~printer:string_of_int;
  assert_equal 17 (interp s8) ~msg:"Two Jmps to label" ~printer:string_of_int;
  assert_equal 42 (interp s9) ~msg:"Movq RSP to RAX" ~printer:string_of_int;
  assert_equal 42 (interp s10) ~msg:"Two labels" ~printer:string_of_int;
  assert_equal 23 (interp s11) ~msg:"Ref var" ~printer:string_of_int;
  assert_equal 52 (interp ~input:[ 52 ] s12) ~msg:"Callq 'read'" ~printer:string_of_int;
  assert_equal 4 (interp s13) ~msg:"Override register" ~printer:string_of_int;
  assert_equal 10 (interp s14) ~msg:"Addq same register" ~printer:string_of_int;
  assert_equal (-5) (interp s15) ~msg:"Pushq Popq Pushq Popq" ~printer:string_of_int;
  assert_equal 42 (interp s16) ~msg:"Movq after Retq" ~printer:string_of_int

let suite = "x0_tests" >::: [ "test" >:: test ]

let _ = run_test_tt_main suite
