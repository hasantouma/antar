open Clang
open Xlang

let var_of_stmt (s : stmt) : xvar =
  match s with
  | Set (x, _) -> x

let rec get_vars (acc : cvar list) (tail : tail) : xvar list =
  match tail with
  | Return _ -> acc
  | Seq (s, t) ->
    let x = var_of_stmt s in
    get_vars (x :: acc) t

let uncover_locals (cprog : cprogram) : cprogram =
  let tail = List.assoc "entry" cprog.blks in
  let info' = get_vars [] tail in
  { cprog with info = info' }

let select_arg (arg : carg) : xarg =
  match arg with
  | Number n -> Constant n
  | Var s -> Ref s

let select_exp (dst : xarg) (exp : exp) : instr list =
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

let select_stmt (stmt : stmt) : instr list =
  match stmt with
  | Set (var, exp) ->
    let arg_val = select_arg (Var var) in
    select_exp arg_val exp

let rec select_tail (tail : tail) : instr list =
  match tail with
  | Return arg ->
    let arg' = select_arg arg in
    [ Movq (arg', Reg RAX) ]
  | Seq (stmt, tail) -> select_stmt stmt @ select_tail tail

let select_instr (cprog : cprogram) : xprogram =
  let p_with_locals = uncover_locals cprog in
  let entry_tail : tail = List.assoc "entry" p_with_locals.blks in
  let instrs = select_tail entry_tail in
  let vars_length = stack_space p_with_locals.info in
  let instrs' = wrap_x_helper ~vars_length instrs in
  let block = { info = []; instrs = instrs' } in
  { info = p_with_locals.info; blks = [ ("entry", block) ] }
