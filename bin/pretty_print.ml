open Lex_parse.Ast

let pp_all f expr =
  match expr with
  | #R0.Ast.expr as y -> R0.Pp.pp f y

let rec pp (expr : expr): string = pp_all pp expr

