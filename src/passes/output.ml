type pass =
  | Optimize
  | Uniquify
  | ResolveComplex
  | ExplicateControl
  | SelectInstr
  | AssignHomes
  | PatchInstrs

(* Up-to pass *)
let upto_optimize rprog = rprog |> Optimize.optimize
let upto_uniquify rprog = rprog |> upto_optimize |> Uniquify.uniquify
let upto_resolve_complex rprog = rprog |> upto_uniquify |> Resolve_complex.resolve_complex
let upto_explicate_control rprog = rprog |> upto_resolve_complex |> Explicate_control.explicate_control
let upto_select_instr rprog = rprog |> upto_explicate_control |> Select_instr.select_instr
let upto_assign_homes rprog = rprog |> upto_select_instr |> Assign_homes.assign_homes
let upto_patch_instrs rprog = rprog |> upto_assign_homes |> Patch_instructions.patch_instructions

(* Run rprog up to the given pass and return the pretty-print in the output lang *)
let string_of_pass (rprog : Rlang.rprogram) (pass : pass) : string * string =
  match pass with
  | Optimize ->
    let opt = upto_optimize rprog in
    ("opt", Rlang.pp opt)
  | Uniquify ->
    let uni = upto_uniquify rprog in
    ("uni", Rlang.pp uni)
  | ResolveComplex ->
    let rco = upto_resolve_complex rprog in
    ("rco", Rlang.pp rco)
  | ExplicateControl ->
    let econ = upto_explicate_control rprog in
    ("econ", Clang.pp econ)
  | SelectInstr ->
    let si = upto_select_instr rprog in
    ("si", Xlang.emitp true si)
  | AssignHomes ->
    let ah = upto_assign_homes rprog in
    ("ah", Xlang.emitp true ah)
  | PatchInstrs ->
    let pi = upto_patch_instrs rprog in
    ("pi", Xlang.emitp false pi)

let string_of_passes (rprog : Rlang.rprogram) (passes : pass list) : (string * string) list =
  List.map (string_of_pass rprog) passes

let output_passes ~(input_file : string) ((pass_name, pass_string) : string * string) : unit =
  let output_file = input_file ^ "." ^ pass_name ^ ".ht" in
  let output_channel = open_out output_file in
  output_string output_channel pass_string;
  close_out output_channel

(* Run rprog up to the given pass and output the pretty-print of the result to <input_file>.<pass_name>.ht *)
let process_passes ~(input_file : string) ~(rprog : Rlang.rprogram) ~(passes : pass list) : unit =
  let passes_pair : (string * string) list = string_of_passes rprog passes in
  List.iter (output_passes ~input_file) passes_pair
