let lang_name = Printf.sprintf "Antar %s" Rlang.name
let usage_msg = Printf.sprintf "'%s' programming language" lang_name
let greetings = Printf.sprintf "Welcome to the '%s' REPL" lang_name

(* hcc helpers *)
let assembly_ref = ref false
let input_files_ref = ref []
let output_file_ref = ref "a.out"
let append_input_files_list filename = input_files_ref := filename :: !input_files_ref

(* passes flags *)
let optimize_ref = ref false
let uniquify_ref = ref false
let resolve_complex_ref = ref false
let explicate_control_ref = ref false
let select_instr_ref = ref false
let assign_homes_ref = ref false
let patch_instrs_ref = ref false
let passes_list_ref = ref []
let append_passes_list (pass : Passes.pass) : unit = passes_list_ref := pass :: !passes_list_ref

let get_passes_list () : Passes.pass list =
  if !optimize_ref then append_passes_list Passes.Optimize;
  if !uniquify_ref then append_passes_list Passes.Uniquify;
  if !resolve_complex_ref then append_passes_list Passes.ResolveComplex;
  if !explicate_control_ref then append_passes_list Passes.ExplicateControl;
  if !select_instr_ref then append_passes_list Passes.SelectInstr;
  if !assign_homes_ref then append_passes_list Passes.AssignHomes;
  if !patch_instrs_ref then append_passes_list Passes.PatchInstrs;
  !passes_list_ref

let parse_cmd_line_args () =
  let speclist =
    [ ("-f", Arg.String Repl.interp_file, "<file_path> Parsing file")
    ; ("-g", Arg.Int (Repl.randp false), "<int> Generate random program of size n")
    ; ("-gv", Arg.Int (Repl.randp true), "<int> Generate, and visualize, random program of size n")
    ; ("-v", Arg.String Repl.visualize, "<file_path> File to visualize. Will output a 'mygraph.png' file.")
    ; ("-S", Arg.Set assembly_ref, "Output X86 assembly file as <input_file>.s")
    ; ("-o", Arg.Set_string output_file_ref, "Set output file name")
    ; ("-opt", Arg.Set optimize_ref, "Output rlang file with Optimize pass as <input_file>.opt.ht")
    ; ("-uni", Arg.Set uniquify_ref, "Output rlang file with Uniquify pass as <input_file>.uni.ht")
    ; ("-rco", Arg.Set resolve_complex_ref, "Output rlang file with Resolve-Complex pass as <input_file>.rco.ht")
    ; ("-econ", Arg.Set explicate_control_ref, "Output clang file with Explicate-Control pass as <input_file>.econ.ht")
    ; ("-si", Arg.Set select_instr_ref, "Output xlang file with Select-Instruction pass as <input_file>.si.ht")
    ; ("-ah", Arg.Set assign_homes_ref, "Output xlang file with Assign-Homes pass as <input_file>.ah.ht")
    ; ("-pi", Arg.Set patch_instrs_ref, "Output xlang file with Patch-Instructions pass as <input_file>.pi.ht")
    ]
  in
  Arg.parse speclist append_input_files_list usage_msg;
  let passes : Passes.pass list = get_passes_list () in
  let output_assembly = !assembly_ref in
  let output_file = !output_file_ref in
  if List.length !input_files_ref > 0
  then
    let input_file = List.hd !input_files_ref in
    Repl.compile ~input_file ~output_file ~output_assembly ~passes
  else ()

let run () =
  let args_len = Array.length Sys.argv in
  if args_len > 1
  then parse_cmd_line_args ()
  else (
    print_endline greetings;
    Repl.repl ())

let () = run ()
