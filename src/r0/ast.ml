
type 'a expr = [
  | `EInt of int
  | `ERead
  | `ENegate of 'a
  | `EAdd of 'a * 'a
]

