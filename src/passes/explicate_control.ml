open C0.Ast
open R1.Ast

let explicate_control (expr : expr) : p =
  match expr with
  | _ -> { info = false; blks = [] }
