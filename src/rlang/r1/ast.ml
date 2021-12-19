type var = string

type expr =
  [ `EInt of int
  | `ERead
  | `ENegate of expr
  | `EAdd of expr * expr
  | `EVar of var
  | `ELet of var * expr * expr
  ]
