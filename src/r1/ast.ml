
type 'a expr_open = [
  | 'a R0_ast.expr_open
  | `EMult of 'a * 'a
]

type expr = expr expr_open

