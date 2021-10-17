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
  { info : bool
  ; blks : (label * tail) list
  }
