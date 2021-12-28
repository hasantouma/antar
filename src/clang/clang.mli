type cvar = string
type label = string

type carg =
  | Number of int
  | Var of string

type exp =
  | Arg of carg
  | Read
  | Negate of carg
  | Add of carg * carg

type stmt = Set of cvar * exp

type tail =
  | Return of carg
  | Seq of stmt * tail

type cprogram =
  { info : cvar list
  ; blks : (label * tail) list
  }

type env =
  { assoc : (string * int) list
  ; read_int : unit -> int
  }

val interp : ?inputs:int list -> cprogram -> int
val pp : cprogram -> string
val make_cprog : (label * tail) list -> cprogram
val wrap_c_entry : tail -> cprogram
