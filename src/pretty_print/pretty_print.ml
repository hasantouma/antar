open Lex_parse.Ast

let pp_all (f : expr -> string) (expr : expr) : string =
  match expr with
  | #R0.Ast.expr as y -> R0.Pp.pp f y

let rec pp (expr : expr) : string = pp_all pp expr

let pp_with_indent (expr : expr) : string =
  Sexpr.string_of_sexpr_indent (Sexpr.parse_string (pp expr))

