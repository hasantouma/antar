let parse_cmd_line_args () =
  let speclist =
    [ ("-f", Arg.String Repl.interp_file, "<file_path> Parsing file")
    ; ("-g", Arg.Int (Repl.randp false), "<int> Generate random program of size n")
    ; ("-gv", Arg.Int (Repl.randp true), "<int> Generate, and visualize, random program of size n")
    ; ("-v", Arg.String Repl.visualize, "<file_path> File to visualize")
    ]
  in
  let usage_msg = "'Antar' programming language" in
  Arg.parse speclist print_endline usage_msg

let () =
  let args_len = Array.length Sys.argv in
  if args_len > 1 then
    parse_cmd_line_args ()
  else (
    print_endline "Welcome to the 'Antar' REPL";
    Repl.repl ()
  )
