open Lex_parse.Ast

let interp_all (f : expr -> (unit -> int) -> int) (expr : expr) (read_int : (unit -> int)) : int =
  match expr with
  | #R0.Ast.expr as expr -> R0.Interp.interp f expr read_int

let rec interp (expr : expr) (read_int : unit -> int) : int =
  interp_all interp expr read_int

