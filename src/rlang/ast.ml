type rvar = string

type expr =
  | EInt of int
  | ERead
  | ENegate of expr
  | EAdd of expr * expr
  | EVar of rvar
  | ELet of rvar * expr * expr

type rprogram =
  { info : bool
  ; e : expr
  }
