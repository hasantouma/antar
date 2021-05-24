open Ast
open Lexer
open Parser

let parse_file name =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let (p : program) = prog token lexbuf in
    close_in chan;
    p

(* not currently working *)
let parse_stdin () =
  let lexbuf = Lexing.from_channel stdin in
  let result = prog token lexbuf in
  Printf.printf "Finished result!\n";
  result

