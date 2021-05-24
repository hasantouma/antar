open Pretty_print

let () =
  let p = Lex_parse.Main.parse_file "ex1.ht" in
  print_endline (pp p.e)

