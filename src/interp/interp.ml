open Lex_parse.Ast

let interp_all (f : expr -> (unit -> int) -> int) (expr : expr) (read_int : (unit -> int)) : int =
  match expr with
  | #R0.Ast.expr as expr -> R0.Interp.interp f expr read_int

let rec interp_helper (expr : expr) (read_int : unit -> int) : int =
  interp_all interp_helper expr read_int

let interp (expr : expr) (inputs : int list) : int =
  interp_helper expr (Utils.make_read inputs)

