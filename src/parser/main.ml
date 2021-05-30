open Ast
open Lexer
open Parser

let make_prog (lexbuf : Lexing.lexbuf) : program = { info = false; e = expr_start token lexbuf }

let parse_file name =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let (p : program) = make_prog lexbuf in
    close_in chan;
    p

