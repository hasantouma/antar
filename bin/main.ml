open Pretty_print
open Interp
open Repl
open Generator

let interp_randp (n : int) : unit =
  let r = randp n in
  print_endline (pp_with_indent r);
  let p = interp r [] in
  let s = string_of_int p in
  print_endline s


let () =
  let speclist = [
    ("-f", Arg.String interp_file, "Parsing file");
    ("-g", Arg.Int interp_randp, "Generate random program of size n")
  ] in
  let usage_msg = "'Antar' programming language" in
  Arg.parse speclist print_endline usage_msg



let () =
  let args_len = Array.length Sys.argv in
  if args_len = 1 then
    begin
      print_endline "Welcome to the 'Antar' REPL";
      repl ()
    end

