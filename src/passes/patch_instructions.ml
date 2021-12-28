open Xlang

let patch_instructions_instr (instr : instr) : instr list =
  match instr with
  | Addq ((Deref _ as src), (Deref _ as dst)) -> [ Movq (src, Reg RAX); Addq (Reg RAX, dst) ]
  | Subq ((Deref _ as src), (Deref _ as dst)) -> [ Movq (src, Reg RAX); Subq (Reg RAX, dst) ]
  | Movq ((Deref _ as src), (Deref _ as dst)) -> [ Movq (src, Reg RAX); Movq (Reg RAX, dst) ]
  | _ -> [ instr ]

let patch_instructions_instrs (instrs : instr list) : instr list =
  List.map patch_instructions_instr instrs |> List.flatten

let patch_instructions_block (block : block) : block =
  let instrs' = patch_instructions_instrs block.instrs in
  { block with instrs = instrs' }

let patch_instructions_blks ((label, block) : label * block) : label * block =
  let block' = patch_instructions_block block in
  (label, block')

let patch_instructions (xprog : xprogram) : xprogram =
  let blks' = List.map patch_instructions_blks xprog.blks in
  { xprog with blks = blks' }

(* is_patch_instructions *)
let is_patch_instructions_instr (b : bool) (instr : instr) : bool =
  let res =
    match instr with
    | Addq (Deref _, Deref _) | Subq (Deref _, Deref _) | Movq (Deref _, Deref _) -> false
    | _ -> true
  in
  b && res

let is_patch_instructions_instrs (b : bool) (instrs : instr list) : bool =
  List.fold_left is_patch_instructions_instr b instrs

let is_patch_instructions_block (b : bool) (block : block) : bool = is_patch_instructions_instrs b block.instrs
let is_patch_instructions_blks (b : bool) ((_, block) : label * block) : bool = is_patch_instructions_block b block
let is_patch_instructions (xprog : xprogram) : bool = List.fold_left is_patch_instructions_blks true xprog.blks
