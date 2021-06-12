
type 'a expr_open = [
  | 'a R0.Ast.expr_open
  | `EMult of 'a * 'a
]

type expr = expr expr_open

