open Ast
open Utils.Generator

let rec exp2 (n : int) : r0 =
  match n with
  | 0 -> `EInt 1
  | n ->
      let result = exp2 (n - 1) in
      `EAdd(result, result)

let num_of_reads_open f expr =
  match expr with
  | `EInt _ -> 0
  | `ERead -> 1
  | `EAdd(l, r) -> (f l) + (f r)
  | `ENegate e -> f e

let rec num_of_reads expr = num_of_reads_open num_of_reads expr

let rec randp_open func n =
  if n = 0 then
    let random_int = next_int () in
    match next_float () with
    | f when f < 0.5 -> `ERead
    | _ -> `EInt random_int
  else
    match next_float () with
    | f when f < 0.5 -> `ENegate (randp_open func (n - 1))
    | _ ->
        let left = randp_open func (n - 1) in
        let right = randp_open func (n - 1) in
        `EAdd (left, right)

let rec randp n = randp_open randp n

let generate_input_for_randp expr : int list =
  let reads : int = num_of_reads expr in
  generate_input reads

