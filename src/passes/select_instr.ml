open X0.Ast

(* Helper - copied from 'tests/xlang/x0/x0_tests.ml' *)
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

let var_of_stmt (s : C0.Ast.stmt) : X0.Ast.var =
  match s with
  | Set (x, _) -> x

let rec get_vars (acc : C0.Ast.var list) (tail : C0.Ast.tail) : X0.Ast.var list =
  match tail with
  | Return _ -> acc
  | Seq (s, t) ->
    let x = var_of_stmt s in
    get_vars (x :: acc) t

let uncover_locals (p : C0.Ast.cprogram) : C0.Ast.cprogram =
  let tail = List.assoc "entry" p.blks in
  let info' = get_vars [] tail in
  { p with info = info' }

let select_arg (arg : C0.Ast.arg) : X0.Ast.arg =
  match arg with
  | Number n -> Constant n
  | Var s -> Ref s

let select_exp (dst : X0.Ast.arg) (exp : C0.Ast.exp) : X0.Ast.instr list =
  match exp with
  | Arg arg ->
    let arg' = select_arg arg in
    [ Movq (arg', dst) ]
  | Read -> [ Callq "read_int"; Movq (Reg RAX, dst) ]
  | Negate arg ->
    let arg' = select_arg arg in
    [ Movq (arg', dst); Negq dst ]
  | Add (al, ar) ->
    let al' = select_arg al in
    let ar' = select_arg ar in
    [ Movq (ar', dst); Addq (al', dst) ]

let select_stmt (stmt : C0.Ast.stmt) : X0.Ast.instr list =
  match stmt with
  | Set (var, exp) ->
    let arg_val = select_arg (Var var) in
    select_exp arg_val exp

let rec select_tail (tail : C0.Ast.tail) : X0.Ast.instr list =
  match tail with
  | Return arg ->
    let arg' = select_arg arg in
    [ Movq (arg', Reg RAX) ]
  | Seq (stmt, tail) -> select_stmt stmt @ select_tail tail

let select_instr (p : C0.Ast.cprogram) : X0.Ast.xprogram =
  let p_with_locals = uncover_locals p in
  let entry_tail : C0.Ast.tail = List.assoc "entry" p_with_locals.blks in
  let instrs = select_tail entry_tail in
  let vars_length = stack_space p_with_locals.info in
  let instrs' = wrap vars_length instrs in
  let block = { info = []; instrs = instrs' } in
  { info = p_with_locals.info; blks = [ ("entry", block) ] }
