(* uniquify *)
let uniquify = Uniquify.uniquify
let is_uniquify = Uniquify.is_uniquify

(* resolve_complex *)
let resolve_complex = Resolve_complex.resolve_complex
let is_resolve_complex = Resolve_complex.is_resolve_complex

(* explicate_control *)
let explicate_control = Explicate_control.explicate_control

(* select_instr *)
let uncover_locals = Select_instr.uncover_locals
let select_instr = Select_instr.select_instr

(* assign_homes *)
let assign_homes = Assign_homes.assign_homes
let is_assign_homes = Assign_homes.is_assign_homes

(* patch_instructions *)
let patch_instructions = Patch_instructions.patch_instructions
let is_patch_instructions = Patch_instructions.is_patch_instructions
