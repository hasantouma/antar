open Interp
open Ast
open Parser.Main
open Pretty_print

let parse_file (name : string) : program =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let p : program = make_prog lexbuf in
  close_in chan;
  p

let handle_exit repl_in =
  if repl_in = "#quit" then
    exit 0

let rec repl () : unit =
  output_string stdout "> ";
  flush stdout;
  let repl_in = (input_line stdin) in
  handle_exit repl_in;
  let lexbuf = Lexing.from_string repl_in in
  let p : program = make_prog lexbuf in
  print_endline (string_of_int (interp p.e []));
  repl ()

let interp_file (file_name : string) : unit =
  let p : program = parse_file file_name in
  print_endline (pp_with_indent p.e);
  let i = interp p.e [] in
  print_endline (string_of_int i)

