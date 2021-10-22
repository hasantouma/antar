open R_utils.Generator

let num_of_reads_open f expr =
  match expr with
  | `EVar _ -> 0
  | `ELet (_, ex, eb) ->
    let vx = f ex in
    let vb = f eb in
    vx + vb
  | #R0.Ast.expr_open as e -> R0.Generator.num_of_reads_open f e

let rec num_of_reads expr = num_of_reads_open num_of_reads expr

let choice func vars =
  match vars with
  | [] -> R0.Generator.randp_open func vars 0
  | _ -> (
    match next_float () with
    | f when f < 0.66 -> R0.Generator.randp_open func vars 0
    | _ ->
      let rand_index = next_int ~bound:(List.length vars) () in
      let v = List.nth vars rand_index in
      `EVar v)

let choose_vars vars =
  if List.length vars > 0 && next_float () < 0.5 then
    vars
  else
    let x = Utils.Fresh.fresh_var () in
    x :: vars

let randp_open func vars n =
  if n = 0 then
    choice func vars
  else
    match next_float () with
    | f when f < 0.66 -> R0.Generator.randp_open func vars n
    | _ ->
      let vars' = choose_vars vars in
      let x = List.hd vars' in
      let ex = func vars (n - 1) in
      let eb = func vars' (n - 1) in
      `ELet (x, ex, eb)

let randp ?(vars = []) n =
  let rec randp vars n = randp_open randp vars n in
  randp vars n

let generate_input_for_randp expr : int list =
  let reads : int = num_of_reads expr in
  generate_input reads
