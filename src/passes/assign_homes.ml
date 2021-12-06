open X0.Ast

let var_reg_map (index : int) (var : var) : var * arg =
  let offset = -1 * 8 * (index + 1) in
  (var, Deref (RSP, offset))

let assign_homes_arg (lst : (var * arg) list) (arg : arg) : arg =
  match arg with
  | Constant n -> Constant n
  | Reg r -> Reg r
  | Deref (r, off) -> Deref (r, off)
  | Ref x -> List.assoc x lst

let assign_homes_instr (lst : (var * arg) list) (instr : instr) : instr =
  match instr with
  | Addq (al, ar) ->
    let al' = assign_homes_arg lst al in
    let ar' = assign_homes_arg lst ar in
    Addq (al', ar')
  | Subq (al, ar) ->
    let al' = assign_homes_arg lst al in
    let ar' = assign_homes_arg lst ar in
    Subq (al', ar')
  | Movq (al, ar) ->
    let al' = assign_homes_arg lst al in
    let ar' = assign_homes_arg lst ar in
    Movq (al', ar')
  | Retq -> Retq
  | Negq a ->
    let a' = assign_homes_arg lst a in
    Negq a'
  | Callq l -> Callq l
  | Jmp l -> Jmp l
  | Pushq a ->
    let a' = assign_homes_arg lst a in
    Pushq a'
  | Popq a ->
    let a' = assign_homes_arg lst a in
    Popq a'

let assign_homes_instrs (lst : (var * arg) list) (instrs : instr list) : instr list =
  List.map (assign_homes_instr lst) instrs

let assign_homes_block (lst : (var * arg) list) (block : block) : block =
  let instrs' = assign_homes_instrs lst block.instrs in
  { block with instrs = instrs' }

let assign_homes_blks (lst : (var * arg) list) ((label, block) : label * block) : label * block =
  let block' = assign_homes_block lst block in
  (label, block')

let assign_homes (p : xprogram) : xprogram =
  let var_reg_lst = List.mapi var_reg_map p.info in
  let blks' = List.map (assign_homes_blks var_reg_lst) p.blks in
  { p with blks = blks' }

let is_assign_homes_arg (arg : arg) : bool =
  match arg with
  | Constant _
  | Reg _
  | Deref _ ->
    true
  | Ref _ -> false

let is_assign_homes_instr (b : bool) (instr : instr) : bool =
  let res =
    match instr with
    | Addq (al, ar)
    | Subq (al, ar)
    | Movq (al, ar) ->
      is_assign_homes_arg al && is_assign_homes_arg ar
    | Retq -> true
    | Negq a
    | Pushq a
    | Popq a ->
      is_assign_homes_arg a
    | Callq _
    | Jmp _ ->
      true
  in
  b && res

let is_assign_homes_instrs (b : bool) (instrs : instr list) : bool = List.fold_left is_assign_homes_instr b instrs

let is_assign_homes_block (b : bool) (block : block) : bool = is_assign_homes_instrs b block.instrs

let is_assign_homes_blks (b : bool) ((_, block) : label * block) : bool = is_assign_homes_block b block

let is_assign_homes (p : xprogram) : bool = List.fold_left is_assign_homes_blks true p.blks