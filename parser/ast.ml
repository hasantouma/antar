
type 'a expr_all = [
  | 'a R0.Ast.expr
]

type expr = expr expr_all

type program = { info : bool;
  e : expr }

