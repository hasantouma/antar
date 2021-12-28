type xvar = string
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

type xarg =
  | Constant of int
  | Reg of register
  | Deref of register * int
  | Ref of xvar

type instr =
  | Addq of xarg * xarg
  | Subq of xarg * xarg
  | Movq of xarg * xarg
  | Retq
  | Negq of xarg
  | Callq of label
  | Jmp of label
  | Pushq of xarg
  | Popq of xarg

type block =
  { info : xvar list
  ; instrs : instr list
  }

type xprogram =
  { info : xvar list
  ; blks : (label * block) list
  }

type ms =
  { regs : (register * int) list
  ; addrs : (int * int) list
  ; vars : (xvar * int) list
  ; labels : (label * block) list
  ; read_int : unit -> int
  }

val assemble : ?inputs:string list -> xprogram -> string
val emitp : bool -> xprogram -> string
val interp : ?inputs:int list -> xprogram -> int
val make_xprog : ?pinfo:xvar list -> (label * instr list) list -> xprogram
val stack_space : xvar list -> int
val wrap_x_helper : ?vars_length:int -> instr list -> instr list
val wrap_x_entry : ?pinfo:xvar list -> instr list -> xprogram
