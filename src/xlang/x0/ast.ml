type var = string

type label = string

type register =
  | RSP
  | RBP
  | RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type arg =
  | Constant of int
  | Reg of register
  | Deref of register * int
  | Ref of var

type instr =
  | Addq of arg * arg
  | Subq of arg * arg
  | Movq of arg * arg
  | Retq
  | Negq of arg
  | Callq of label
  | Jmp of label
  | Pushq of arg
  | Popq of arg

type block =
  { info : var list
  ; instrs : instr list
  }

type xprogram =
  { info : var list
  ; blks : (label * block) list
  }

type ms =
  { regs : (register * int) list
  ; addrs : (int * int) list
  ; vars : (var * int) list
  ; labels : (label * block) list
  ; read_int : unit -> int
  }
