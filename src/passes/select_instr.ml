open Xlang

let var_of_stmt (s : Clang.stmt) : Xlang.var =
  match s with
  | Set (x, _) -> x

let rec get_vars (acc : Clang.var list) (tail : Clang.tail) : Xlang.var list =
  match tail with
  | Return _ -> acc
  | Seq (s, t) ->
    let x = var_of_stmt s in
    get_vars (x :: acc) t

let uncover_locals (p : Clang.cprogram) : Clang.cprogram =
  let tail = List.assoc "entry" p.blks in
  let info' = get_vars [] tail in
  { p with info = info' }

let select_arg (arg : Clang.arg) : Xlang.arg =
  match arg with
  | Number n -> Constant n
  | Var s -> Ref s

let select_exp (dst : Xlang.arg) (exp : Clang.exp) : Xlang.instr list =
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

let select_stmt (stmt : Clang.stmt) : Xlang.instr list =
  match stmt with
  | Set (var, exp) ->
    let arg_val = select_arg (Var var) in
    select_exp arg_val exp

let rec select_tail (tail : Clang.tail) : Xlang.instr list =
  match tail with
  | Return arg ->
    let arg' = select_arg arg in
    [ Movq (arg', Reg RAX) ]
  | Seq (stmt, tail) -> select_stmt stmt @ select_tail tail

let select_instr (p : Clang.cprogram) : Xlang.xprogram =
  let p_with_locals = uncover_locals p in
  let entry_tail : Clang.tail = List.assoc "entry" p_with_locals.blks in
  let instrs = select_tail entry_tail in
  let vars_length = Xlang.stack_space p_with_locals.info in
  let instrs' = Xlang.wrap ~vars_length instrs in
  let block = { info = []; instrs = instrs' } in
  { info = p_with_locals.info; blks = [ ("entry", block) ] }
