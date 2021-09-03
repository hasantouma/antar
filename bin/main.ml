
let interp_randp (n : int) : unit =
  let r = R1.Generator.randp n in
  print_endline (R1.Pp.pp r 0);
  let p = R1.Interp.interp r [] (Utils.Repl.make_read []) in
  let s = string_of_int p in
  print_endline s


let parse_cmd_line_args () =
  let speclist = [
    ("-f", Arg.String Repl.interp_file, "<file_path> Parsing file");
    ("-g", Arg.Int interp_randp, "<int> Generate random program of size n");
    ("-v", Arg.String Repl.visualize, "<file_path> File to visualize")
  ] in
  let usage_msg = "'Antar' programming language" in
  Arg.parse speclist print_endline usage_msg



let () =
  let args_len = Array.length Sys.argv in
  if args_len > 1 then
    parse_cmd_line_args ()
  else
    begin
      print_endline "Welcome to the 'Antar' REPL";
      Repl.repl ()
    end

