type var = string

type 'a expr_open =
  [ 'a R0.Ast.expr_open
  | `EVar of var
  | `ELet of var * 'a * 'a
  ]

type expr = expr expr_open

type program =
  { info : bool
  ; e : expr
  }
