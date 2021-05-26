open Pretty_print

let () =
  let p = Lex_parse.Main.parse_file "src/ex1.ht" in
  print_endline (pp p.e)

