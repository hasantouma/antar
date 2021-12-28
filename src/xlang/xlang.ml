include Ast

let assemble = Assemble.assemble
let emitp = Emit.emitp
let interp = Interp.interp

let make_xprog ?(pinfo = []) (lst : (label * instr list) list) : xprogram =
  let blocks =
    List.map
      (fun (label, instrs) ->
        let block = { info = []; instrs } in
        (label, block))
      lst
  in
  { info = pinfo; blks = blocks }

let stack_space (lst : xvar list) : int =
  let n = List.length lst in
  if n mod 2 = 0 then 8 * n else 8 * (n + 1)

let wrap_x_helper ?(vars_length = 0) (lst : instr list) : instr list =
  let sub = [ Subq (Constant vars_length, Reg RSP) ] in
  let add = [ Addq (Constant vars_length, Reg RSP) ] in
  let prologue = [ Pushq (Reg RBP); Movq (Reg RSP, Reg RBP) ] in
  let epilogue = [ Popq (Reg RBP); Retq ] in
  if vars_length = 0
  then List.append (List.append prologue lst) epilogue
  else
    let prologue' = prologue @ sub in
    let epilogue' = add @ epilogue in
    List.append (List.append prologue' lst) epilogue'

let wrap_x_entry ?(pinfo = []) (instrs : instr list) : xprogram =
  let vars_length = stack_space pinfo in
  let entry = wrap_x_helper ~vars_length instrs in
  make_xprog ~pinfo [ ("entry", entry) ]
