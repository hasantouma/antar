type var = string
type label = string

type arg =
  | Number of int
  | Var of string

type exp =
  | Arg of arg
  | Read
  | Negate of arg
  | Add of arg * arg

type stmt = Set of var * exp

type tail =
  | Return of arg
  | Seq of stmt * tail

type cprogram =
  { info : var list
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
