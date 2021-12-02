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

type p =
  { info : var list
  ; blks : (label * tail) list
  }

type env =
  { assoc : (string * int) list
  ; read_int : unit -> int
  }
