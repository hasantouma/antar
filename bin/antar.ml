let lang_name = Printf.sprintf "Antar %s" Rlang.name
let usage_msg = Printf.sprintf "'%s' programming language" lang_name
let greetings = Printf.sprintf "Welcome to the '%s' REPL" lang_name

(* hcc helpers *)
let input_files_ref = ref []
let output_file_ref = ref ""
let anon_fun filename = input_files_ref := filename :: !input_files_ref

let parse_cmd_line_args () =
  let speclist =
    [ ("-f", Arg.String Repl.interp_file, "<file_path> Parsing file")
    ; ("-g", Arg.Int (Repl.randp false), "<int> Generate random program of size n")
    ; ("-gv", Arg.Int (Repl.randp true), "<int> Generate, and visualize, random program of size n")
    ; ("-v", Arg.String Repl.visualize, "<file_path> File to visualize")
      (*; ("-c", Arg.String Repl.compile, "<file_path> Output the X86 assembly code") TODO: Remove when done *)
    ; ("-o", Arg.Set_string output_file_ref, "Set output file name")
    ]
  in
  Arg.parse speclist anon_fun usage_msg;
  print_endline ("Inputs: " ^ [%show: string list] !input_files_ref);
  print_endline ("Output: " ^ !output_file_ref);
  let output_file = if !output_file_ref = "" then "a.out" else !output_file_ref in
  if List.length !input_files_ref > 0
  then
    let input_file = List.hd !input_files_ref in
    Repl.compile ~input_file ~output_file
  else ()

let run () =
  let args_len = Array.length Sys.argv in
  if args_len > 1
  then parse_cmd_line_args ()
  else (
    print_endline greetings;
    Repl.repl ())

let () = run ()
