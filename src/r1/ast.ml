type var = string

type 'a r1_open =
  [ 'a R0.Ast.r0_open
  | `EVar of var
  | `ELet of var * 'a * 'a
  ]

type r1 = r1 r1_open
