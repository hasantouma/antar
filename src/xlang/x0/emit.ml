open Ast

let emitr (reg : register) : string =
  "%"
  ^
  match reg with
  | RSP -> "rsp"
  | RBP -> "rbp"
  | RAX -> "rax"
  | RBX -> "rbx"
  | RCX -> "rcx"
  | RDX -> "rdx"
  | RSI -> "rsi"
  | RDI -> "rdi"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"

let emita (arg : arg) (var : bool) : string =
  match arg with
  | Constant n -> "$" ^ string_of_int n
  | Reg reg -> emitr reg
  | Deref (reg, n) -> string_of_int n ^ "(" ^ emitr reg ^ ")"
  | Ref v ->
    if var then
      v
    else
      raise (Failure "var is false")

let emiti (instr : instr) (var : bool) : string =
  match instr with
  | Addq (src, dst) -> "addq " ^ emita src var ^ ", " ^ emita dst var
  | Subq (src, dst) -> "subq " ^ emita src var ^ ", " ^ emita dst var
  | Movq (src, dst) -> "movq " ^ emita src var ^ ", " ^ emita dst var
  | Retq -> "retq"
  | Negq arg -> "negq " ^ emita arg var
  | Callq label -> "callq " ^ label
  | Jmp label -> "jmp " ^ label
  | Pushq arg -> "pushq " ^ emita arg var
  | Popq arg -> "popq " ^ emita arg var

let emitb (b : block) (var : bool) : string = List.fold_left (fun acc instr -> acc ^ emiti instr var ^ "\n") "" b.instrs

let emitp (p : p) (var : bool) : string =
  let global = ".globl main" in
  List.fold_left (fun acc (label, block) -> acc ^ "\n" ^ label ^ ":\n" ^ emitb block var) global p.blks
