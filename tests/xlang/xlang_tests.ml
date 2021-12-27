open OUnit2
open Xlang

let print_debugger ?(inputs = []) (p : xprogram) (msg : string) : string =
  let input = [%show: string list] inputs in
  let pp = Xlang.emitp true p in
  Printf.sprintf "Input: %s\nTitle: %s:\n%s" input msg pp

let s1 : xprogram = wrap_x_entry [ Movq (Constant 42, Reg RAX) ]
let s2 : xprogram = wrap_x_entry [ Movq (Constant 12, Reg RAX); Addq (Constant 2, Reg RAX) ]
let s3 : xprogram = wrap_x_entry [ Movq (Constant 5, Reg RAX); Subq (Constant 10, Reg RAX) ]
let s4 : xprogram = wrap_x_entry [ Movq (Constant 32, Reg RAX); Movq (Constant 1, Reg RBX); Subq (Reg RBX, Reg RAX) ]
let s5 : xprogram = wrap_x_entry [ Movq (Constant (-57), Reg RAX); Negq (Reg RAX) ]

let s6 : xprogram =
  wrap_x_entry
    [ Movq (Constant 23, Reg RAX)
    ; Pushq (Reg RAX)
    ; Movq (Constant 8, Reg RBX)
    ; Pushq (Reg RBX)
    ; Popq (Reg RAX)
    ; Popq (Reg RBX)
    ; Subq (Reg RBX, Reg RAX)
    ]

let s7 : xprogram =
  let entry = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP); Movq (Constant 42, Reg RAX); Jmp "foo" ] in
  let foo = [ Addq (Constant 1, Reg RAX); Popq (Reg RBP); Retq ] in
  make_xprog [ ("entry", entry); ("foo", foo) ]

let s8 : xprogram =
  let entry = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP); Pushq (Constant 17); Jmp "foo" ] in
  let foo = [ Popq (Reg RBX); Jmp "bar" ] in
  let bar = [ Movq (Reg RBX, Reg RAX); Popq (Reg RBP); Retq ] in
  make_xprog [ ("entry", entry); ("foo", foo); ("bar", bar) ]

let s9 : xprogram = wrap_x_entry [ Pushq (Constant 42); Movq (Deref (RSP, 0), Reg RAX); Addq (Constant 8, Reg RSP) ]

let s10 : xprogram =
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
  make_xprog [ ("start", start); ("entry", entry); ("finish", finish) ]

(* NOTE: Can't test this case against the system assembler because the use of refs *)
let s11 : xprogram =
  let entry = wrap [ Movq (Constant 13, Ref "hi"); Callq "foo" ] in
  let foo = wrap [ Addq (Constant 10, Ref "hi"); Movq (Ref "hi", Reg RAX) ] in
  make_xprog [ ("entry", entry); ("foo", foo) ]

let s12 : xprogram = wrap_x_entry [ Callq "read_int" ]
let s13 : xprogram = wrap_x_entry [ Movq (Constant 5, Reg RAX); Movq (Constant 4, Reg RAX) ]
let s14 : xprogram = wrap_x_entry [ Movq (Constant 5, Reg RAX); Addq (Reg RAX, Reg RAX) ]

let s15 : xprogram =
  wrap_x_entry
    [ Movq (Constant 5, Reg RAX)
    ; Pushq (Reg RAX)
    ; Popq (Reg RBX)
    ; Movq (Constant 6, Reg RAX)
    ; Pushq (Reg RAX)
    ; Popq (Reg RCX)
    ; Addq (Reg RBX, Reg RCX)
    ; Subq (Reg RCX, Reg RAX)
    ]

let s16 : xprogram = make_xprog [ ("entry", [ Movq (Constant 42, Reg RAX); Retq; Movq (Constant 15, Reg RAX) ]) ]

let s17 : xprogram =
  wrap_x_entry [ Callq "read_int"; Movq (Reg RAX, Reg RBX); Callq "read_int"; Subq (Reg RBX, Reg RAX) ]

let s18 : xprogram =
  make_xprog
    [ ( "entry"
      , [ Pushq (Reg RBP)
        ; Movq (Reg RSP, Reg RBP)
        ; Movq (Constant 42, Reg RAX)
        ; Jmp "endme"
        ; Movq (Constant 111, Reg RAX)
        ; Popq (Reg RBP)
        ; Retq
        ] )
    ; ("endme", [ Movq (Constant 1337, Reg RAX); Popq (Reg RBP); Retq ])
    ]

let test_interp _ctxt =
  assert_equal 42 (interp s1) ~msg:(print_debugger s1 "Movq") ~printer:string_of_int;
  assert_equal 14 (interp s2) ~msg:(print_debugger s2 "Addq") ~printer:string_of_int;
  assert_equal (-5) (interp s3) ~msg:(print_debugger s3 "Subq") ~printer:string_of_int;
  assert_equal 31 (interp s4) ~msg:(print_debugger s4 "Subq Regs") ~printer:string_of_int;
  assert_equal 57 (interp s5) ~msg:(print_debugger s5 "Negq") ~printer:string_of_int;
  assert_equal (-15) (interp s6) ~msg:(print_debugger s6 "Pushq and Popq") ~printer:string_of_int;
  assert_equal 43 (interp s7) ~msg:(print_debugger s7 "Jmp to label") ~printer:string_of_int;
  assert_equal 17 (interp s8) ~msg:(print_debugger s8 "Two Jmps to label") ~printer:string_of_int;
  assert_equal 42 (interp s9) ~msg:(print_debugger s9 "Movq RSP to RAX") ~printer:string_of_int;
  assert_equal 42 (interp s10) ~msg:(print_debugger s10 "Two labels") ~printer:string_of_int;
  assert_equal 23 (interp s11) ~msg:(print_debugger s11 "Ref var") ~printer:string_of_int;
  assert_equal 52
    (interp ~inputs:[ 52 ] s12)
    ~msg:(print_debugger ~inputs:[ "52" ] s12 "Callq 'read_int'")
    ~printer:string_of_int;
  assert_equal 4 (interp s13) ~msg:(print_debugger s13 "Override register") ~printer:string_of_int;
  assert_equal 10 (interp s14) ~msg:(print_debugger s14 "Addq same register") ~printer:string_of_int;
  assert_equal (-5) (interp s15) ~msg:(print_debugger s15 "Pushq Popq Pushq Popq") ~printer:string_of_int;
  assert_equal 42 (interp s16) ~msg:(print_debugger s16 "Movq after Retq") ~printer:string_of_int;
  assert_equal 7
    (interp ~inputs:[ 3; 10 ] s17)
    ~msg:(print_debugger ~inputs:[ "3"; "10" ] s17 "Callq 'read_int' twice")
    ~printer:string_of_int;
  assert_equal 1337 (interp s18) ~msg:(print_debugger s18 "Jump and return") ~printer:string_of_int

let test_assemble _ctxt =
  assert_equal "42" (assemble s1) ~msg:(print_debugger s1 "Movq") ~printer:(fun x -> x);
  assert_equal "14" (assemble s2) ~msg:(print_debugger s2 "Addq") ~printer:(fun x -> x);
  assert_equal "-5" (assemble s3) ~msg:(print_debugger s3 "Subq") ~printer:(fun x -> x);
  assert_equal "31" (assemble s4) ~msg:(print_debugger s4 "Subq Regs") ~printer:(fun x -> x);
  assert_equal "57" (assemble s5) ~msg:(print_debugger s5 "Negq") ~printer:(fun x -> x);
  assert_equal "-15" (assemble s6) ~msg:(print_debugger s6 "Pushq and Popq") ~printer:(fun x -> x);
  assert_equal "43" (assemble s7) ~msg:(print_debugger s7 "Jmp to label") ~printer:(fun x -> x);
  assert_equal "17" (assemble s8) ~msg:(print_debugger s8 "Two Jmps to label") ~printer:(fun x -> x);
  assert_equal "42" (assemble s9) ~msg:(print_debugger s9 "Movq RSP to RAX") ~printer:(fun x -> x);
  assert_equal "42" (assemble s10) ~msg:(print_debugger s10 "Two labels") ~printer:(fun x -> x);
  assert_equal "52"
    (assemble ~inputs:[ "52" ] s12)
    ~msg:(print_debugger ~inputs:[ "52" ] s12 "Callq 'read_int'")
    ~printer:(fun x -> x);
  assert_equal "4" (assemble s13) ~msg:(print_debugger s13 "Override register") ~printer:(fun x -> x);
  assert_equal "10" (assemble s14) ~msg:(print_debugger s14 "Addq same register") ~printer:(fun x -> x);
  assert_equal "-5" (assemble s15) ~msg:(print_debugger s15 "Pushq Popq Pushq Popq") ~printer:(fun x -> x);
  assert_equal "42" (assemble s16) ~msg:(print_debugger s16 "Movq after Retq") ~printer:(fun x -> x);
  assert_equal "7"
    (assemble ~inputs:[ "3"; "10" ] s17)
    ~msg:(print_debugger ~inputs:[ "52" ] s17 "Callq 'read_int' twice")
    ~printer:(fun x -> x);
  assert_equal "1337" (assemble s18) ~msg:(print_debugger s18 "Jump and return") ~printer:(fun x -> x)

let suite = "xlang_tests" >::: [ "test_interp" >:: test_interp; "test_assemble" >:: test_assemble ]
let _ = run_test_tt_main suite
