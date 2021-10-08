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

let emita (var : bool) (arg : arg) : string =
  match arg with
  | Constant n -> "$" ^ string_of_int n
  | Reg reg -> emitr reg
  | Deref (reg, n) -> string_of_int n ^ "(" ^ emitr reg ^ ")"
  | Ref v ->
    if var then
      v
    else
      raise (Failure "var is false")

let emiti (var : bool) (instr : instr) : string =
  match instr with
  | Addq (src, dst) -> "addq " ^ emita var src ^ ", " ^ emita var dst
  | Subq (src, dst) -> "subq " ^ emita var src ^ ", " ^ emita var dst
  | Movq (src, dst) -> "movq " ^ emita var src ^ ", " ^ emita var dst
  | Retq -> "retq"
  | Negq arg -> "negq " ^ emita var arg
  | Callq label -> "callq " ^ label
  | Jmp label -> "jmp " ^ label
  | Pushq arg -> "pushq " ^ emita var arg
  | Popq arg -> "popq " ^ emita var arg

let emitb (var : bool) (b : block) : string = List.fold_left (fun acc instr -> acc ^ emiti var instr ^ "\n") "" b.instrs

let emitp (var : bool) (p : p) : string =
  let global = ".globl _entry" in
  List.fold_left (fun acc (label, block) -> acc ^ "\n" ^ label ^ ":\n" ^ emitb var block) global p.blks
