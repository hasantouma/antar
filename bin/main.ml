open Pretty_print
open Lex_parse.Main
open Repl

let () =
  let p = parse_file "src/ex1.ht" in
  print_endline (pp p.e)

let () = repl ()

