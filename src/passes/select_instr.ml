open Xlang.Ast

let var_of_stmt (s : Clang.Ast.stmt) : Xlang.Ast.var =
  match s with
  | Set (x, _) -> x

let rec get_vars (acc : Clang.Ast.var list) (tail : Clang.Ast.tail) : Xlang.Ast.var list =
  match tail with
  | Return _ -> acc
  | Seq (s, t) ->
    let x = var_of_stmt s in
    get_vars (x :: acc) t

let uncover_locals (p : Clang.Ast.cprogram) : Clang.Ast.cprogram =
  let tail = List.assoc "entry" p.blks in
  let info' = get_vars [] tail in
  { p with info = info' }

let select_arg (arg : Clang.Ast.arg) : Xlang.Ast.arg =
  match arg with
  | Number n -> Constant n
  | Var s -> Ref s

let select_exp (dst : Xlang.Ast.arg) (exp : Clang.Ast.exp) : Xlang.Ast.instr list =
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

let select_stmt (stmt : Clang.Ast.stmt) : Xlang.Ast.instr list =
  match stmt with
  | Set (var, exp) ->
    let arg_val = select_arg (Var var) in
    select_exp arg_val exp

let rec select_tail (tail : Clang.Ast.tail) : Xlang.Ast.instr list =
  match tail with
  | Return arg ->
    let arg' = select_arg arg in
    [ Movq (arg', Reg RAX) ]
  | Seq (stmt, tail) -> select_stmt stmt @ select_tail tail

let select_instr (p : Clang.Ast.cprogram) : Xlang.Ast.xprogram =
  let p_with_locals = uncover_locals p in
  let entry_tail : Clang.Ast.tail = List.assoc "entry" p_with_locals.blks in
  let instrs = select_tail entry_tail in
  let vars_length = Xlang.Lang.stack_space p_with_locals.info in
  let instrs' = Xlang.Lang.wrap ~vars_length instrs in
  let block = { info = []; instrs = instrs' } in
  { info = p_with_locals.info; blks = [ ("entry", block) ] }
