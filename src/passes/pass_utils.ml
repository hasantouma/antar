open X0.Ast

let stack_space (lst : C0.Ast.var list) : int =
  let n = List.length lst in
  if n mod 2 = 0 then
    8 * n
  else
    8 * (n + 1)

let wrap (vars_length : int) (lst : X0.Ast.instr list) : X0.Ast.instr list =
  let prologue = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP); Subq (Constant vars_length, Reg RSP) ] in
  let epilogue = [ Addq (Constant vars_length, Reg RSP); Popq (Reg RBP); Retq ] in
  List.append (List.append prologue lst) epilogue
