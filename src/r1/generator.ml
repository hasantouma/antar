open Utils.Generator

let num_of_reads_open f expr =
  match expr with
  | `EMult(l, r) -> (f l) + (f r)
  | #R0.Ast.r0_open as e -> R0.Generator.num_of_reads_open f e

let rec num_of_reads expr = num_of_reads_open num_of_reads expr

let rec randp_open func n =
  if n = 0 then
    R0.Generator.randp_open func n
  else
    match next_float () with
    | f when f < 0.5 ->
        R0.Generator.randp_open func n
    | _ ->
        let left = randp_open func (n - 1) in
        let right = randp_open func (n - 1) in
        `EMult (left, right)

let rec randp n = randp_open randp n

let generate_input_for_randp expr : int list =
  let reads : int = num_of_reads expr in
  generate_input reads

