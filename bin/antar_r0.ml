
let interp_randp (n : int) : unit =
  let r = R0.Generator.randp n in
  print_endline (R0.Pp.pp r 0);
  let p = R0.Interp.interp r (Repl.make_read []) in
  let s = string_of_int p in
  print_endline s


let parse_cmd_line_args () =
  let speclist = [
    ("-f", Arg.String Repl.interp_file, "<file_path> Parsing file");
    ("-g", Arg.Int interp_randp, "<int> Generate random program of size n")
  ] in
  let usage_msg = "'Antar R0' programming language" in
  Arg.parse speclist print_endline usage_msg



let () =
  let args_len = Array.length Sys.argv in
  if args_len > 1 then
    parse_cmd_line_args ()
  else
    begin
      print_endline "Welcome to the 'Antar R0' REPL";
      Repl.repl ()
    end

