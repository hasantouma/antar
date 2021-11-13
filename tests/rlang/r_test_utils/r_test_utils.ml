type rtest =
  { expr : string
  ; optimized : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let make_prog (e : string) = e |> Lexing.from_string |> Parser.Main.make_prog
