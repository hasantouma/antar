open Ast

let make_cprog (lst : (label * tail) list) : cprogram = { info = []; blks = lst }
let wrap_c_entry (tail : tail) : cprogram = make_cprog [ ("entry", tail) ]
