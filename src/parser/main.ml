open Ast
open Lex
open Parse

let make_prog (lexbuf : Lexing.lexbuf) : program =
  {
    info = false;
    e = main token lexbuf
  }

