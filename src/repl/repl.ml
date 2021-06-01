open Interp
open Ast
open Parser.Main

let handle_exit repl_in =
  if repl_in = "#quit" then
    exit 0

let rec repl () : unit =
  output_string stdout "> ";
  flush stdout;
  let repl_in = (input_line stdin) in
  handle_exit repl_in;
  let lexbuf = Lexing.from_string repl_in in
  let (p : program) = make_prog lexbuf in
  print_endline (string_of_int (interp p.e []));
  repl ()

