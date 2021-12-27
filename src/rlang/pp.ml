open Ast

let pp expr =
  let rec pp indent expr =
    match expr with
    | EInt n -> string_of_int n
    | ERead -> "(read)"
    | ENegate e -> "(- " ^ pp indent e ^ ")"
    | EAdd (l, r) ->
      let indent' = indent + 3 in
      let left = pp indent' l in
      let spaces = String.make indent' ' ' in
      let right = pp indent' r in
      "(+ " ^ left ^ "\n" ^ spaces ^ right ^ ")"
    | EVar v -> v
    | ELet (x, ex, eb) ->
      let indent' = indent + 5 in
      let variable_expr = pp indent' ex in
      let body_expr = pp indent' eb in
      let spaces = String.make indent' ' ' in
      "(let ([" ^ x ^ " " ^ variable_expr ^ "])\n" ^ spaces ^ body_expr ^ ")"
  in
  pp 0 expr
