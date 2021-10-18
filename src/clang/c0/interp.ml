open Ast

let interp_a env arg : int =
  match arg with
  | Number n -> n
  | Var v -> List.assoc v env.assoc

let interp_e env exp : int =
  match exp with
  | Arg a -> interp_a env a
  | Read -> env.read_int ()
  | Negate a -> -1 * interp_a env a
  | Add (l, r) -> interp_a env l + interp_a env r

let interp_s env stmt : env =
  match stmt with
  | Set (v, e) ->
    let ans = interp_e env e in
    { env with assoc = (v, ans) :: env.assoc }

let rec interp_t env tail : int =
  match tail with
  | Return a -> interp_a env a
  | Seq (s, t) ->
    let env' = interp_s env s in
    interp_t env' t

let interp ?(inputs = []) (p : p) : int =
  let read_int : unit -> int = Utils.Repl.make_read inputs in
  let env : env = { assoc = []; read_int } in
  let main_tail : tail = List.assoc "main" p.blks in
  interp_t env main_tail
