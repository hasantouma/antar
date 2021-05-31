open Lex_parse.Ast

let rec exp2 (n : int) : expr =
  match n with
  | 0 -> `EInt 1
  | n ->
      let left = exp2 (n - 1) in
      let right = exp2 (n - 1) in
      `EAdd(left, right)

