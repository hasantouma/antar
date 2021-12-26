(* rlang alpha equiv *)
open Rlang.Ast

let rlang_alpha_equiv e1 e2 : bool =
  let rec go env e1 e2 =
    match (e1, e2) with
    | EInt n, EInt m -> n = m
    | ERead, ERead -> true
    | EVar x, EVar y -> (
      let y_opt = List.assoc_opt x env in
      match y_opt with
      | Some y' -> y = y'
      | None -> x = y)
    | ENegate e1', ENegate e2' -> go env e1' e2'
    | EAdd (l1, r1), EAdd (l2, r2) -> go env l1 l2 && go env r1 r2
    | ELet (x, xe, be1), ELet (y, ye, be2) ->
      let env' = (x, y) :: env in
      go env xe ye && go env' be1 be2
    | _ -> false
  in
  go [] e1 e2

(* clang alpha equiv *)
open C0.Ast

let clang_arg_alpha_equiv env a1 a2 : bool =
  match (a1, a2) with
  | Number n, Number m -> n = m
  | Var x, Var y -> (
    let y_opt = List.assoc_opt x env in
    match y_opt with
    | Some y' -> y = y'
    | None -> x = y)
  | _ -> false

let clang_exp_alpha_equiv env exp1 exp2 : bool =
  match (exp1, exp2) with
  | Arg a1, Arg a2 -> clang_arg_alpha_equiv env a1 a2
  | Read, Read -> true
  | Negate a1, Negate a2 -> clang_arg_alpha_equiv env a1 a2
  | Add (l1, r1), Add (l2, r2) -> clang_arg_alpha_equiv env l1 l2 && clang_arg_alpha_equiv env r1 r2
  | _ -> false

let clang_stmt_alpha_equiv env stmt1 stmt2 : (string * string) list * bool =
  match (stmt1, stmt2) with
  | Set (x, exp1), Set (y, exp2) ->
    let env' = (x, y) :: env in
    (env', clang_exp_alpha_equiv env' exp1 exp2)

let rec clang_tail_alpha_equiv env t1 t2 : bool =
  match (t1, t2) with
  | Return a1, Return a2 -> clang_arg_alpha_equiv env a1 a2
  | Seq (stmt1, tail1), Seq (stmt2, tail2) ->
    let env', b = clang_stmt_alpha_equiv env stmt1 stmt2 in
    b && clang_tail_alpha_equiv env' tail1 tail2
  | _ -> false

let clang_alpha_equiv c1 c2 : bool =
  List.fold_left2 (fun acc (_, t1) (_, t2) -> acc && clang_tail_alpha_equiv [] t1 t2) true c1.blks c2.blks
