open R1.Ast

let uniquify (expr : expr) : expr =
  let fresh_var =
    let counter = ref 0 in
    fun () ->
      let x = "x" ^ string_of_int !counter in
      counter := !counter + 1;
      x
  in
  let rec uniquify rho expr =
    match expr with
    | `EInt _
    | `ERead ->
      expr
    | `ENegate e ->
      let e' = uniquify rho e in
      `ENegate e'
    | `EAdd (l, r) ->
      let l' = uniquify rho l in
      let r' = uniquify rho r in
      `EAdd (l', r')
    | `EVar x ->
      let x' = List.assoc x rho in
      `EVar x'
    | `ELet (x, xe, be) ->
      let x' = fresh_var () in
      let xe' = uniquify rho xe in
      let rho' = (x, x') :: rho in
      let be' = uniquify rho' be in
      `ELet (x', xe', be')
  in
  uniquify [] expr
