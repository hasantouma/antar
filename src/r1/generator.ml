open Utils.Generator

let num_of_reads_open f expr =
  match expr with
  | `EVar _ -> 0
  | `ELet (_, ex, eb) ->
    let vx = f ex in
    let vb = f eb in
    vx + vb
  | #R0.Ast.r0_open as e -> R0.Generator.num_of_reads_open f e

let rec num_of_reads expr = num_of_reads_open num_of_reads expr

let randp_open func n =
  if n = 0 then
    R0.Generator.randp_open func n
  else
    match next_float () with
    | f when f < 0.5 -> R0.Generator.randp_open func n
    | _ -> R0.Generator.randp_open func n
(* TODO: Add generator for r1 *)

let rec randp n = randp_open randp n

let generate_input_for_randp expr : int list =
  let reads : int = num_of_reads expr in
  generate_input reads
