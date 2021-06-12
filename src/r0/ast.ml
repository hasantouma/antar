
type 'a expr_open = [
  | `EInt of int
  | `ERead
  | `ENegate of 'a
  | `EAdd of 'a * 'a
]

type expr = expr expr_open

