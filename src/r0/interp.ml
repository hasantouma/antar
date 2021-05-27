
exception Read_input of string

let read_top lst =
  match lst with
  | [] -> raise (Read_input "Empty list!")
  | h :: t -> (h, t)

let interp h expr lst =
  match expr with
  | `EInt n -> (n, lst)
  | `ERead -> read_top lst
  | `ENegate e ->
      let (e', lst') = h e lst in
      let v = (-1) * e' in
      (v, lst')
  | `EAdd(l, r) ->
      let (vl, lst') = h l lst in
      let (vr, lst'') = h r lst' in
      let v = vl + vr in
      (v, lst'')

