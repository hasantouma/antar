{
 open Exceptions
 open Parse
 exception Eof
}

let white = [' ' '\t' '\n' '\r']*
let digits = ('-'?)['0'-'9']+
rule token = parse
  white { token lexbuf }
| "read" { READ }
| '(' { LP }
| ')' { RP }
| '+' { PLUS }
| '-' { NEGATE }
| digits as lxm { INT (int_of_string lxm) }
| eof { EOF }
| _ as lxm { raise (BadInput (Printf.sprintf "Illegal character %c" lxm)) }

