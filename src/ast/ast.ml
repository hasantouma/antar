(*
type 'a expr_all = [
  | 'a R0.Ast.expr
]

type 'a expr_union_r1 = [
  | 'a R0.Ast.expr
  | 'a R1.Ast.expr
]

type expr = expr expr_all

type expr_r0 = expr_r0 R0.Ast.expr
*)
type expr = expr R1.Ast.expr_open

type program = { info : bool;
  e : expr }

