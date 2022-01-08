open Clang
open Rlang
open Xlang

type pass = Output.pass =
  | Optimize
  | Uniquify
  | ResolveComplex
  | ExplicateControl
  | SelectInstr
  | AssignHomes
  | PatchInstrs

(* optimize *)
val optimize : ?env:(string * expr) list -> rprogram -> rprogram

(* uniquify *)
val uniquify : rprogram -> rprogram
val is_uniquify : rprogram -> bool

(* resolve_complex *)
val resolve_complex : rprogram -> rprogram
val is_resolve_complex : rprogram -> bool

(* explicate_control *)
val explicate_control : rprogram -> cprogram

(* select_instr *)
val uncover_locals : cprogram -> cprogram
val select_instr : cprogram -> xprogram

(* assign_homes *)
val assign_homes : xprogram -> xprogram
val is_assign_homes : xprogram -> bool

(* patch_instructions *)
val patch_instructions : xprogram -> xprogram
val is_patch_instructions : xprogram -> bool

(* composition of passes *)
val passes : rprogram -> xprogram

(* Run rprog up to the given pass and output the pretty-print of the result to <input_file>.<pass_name>.ht *)
val process_passes : input_file:string -> rprog:rprogram -> passes:pass list -> unit
