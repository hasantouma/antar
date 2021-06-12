open Ast

let num_of_reads (expr : expr) : int =
  let rec aux expr acc =
    match expr with
    | `EInt _ -> acc
    | `ERead -> 1 + acc
    | `EAdd(l, r) -> (aux l acc) + (aux r acc)
    | `ENegate e -> aux e acc
  in aux expr 0

