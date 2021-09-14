{
 open Exceptions
 open Parse
 exception Eof
}

let white = [' ' '\t' '\n' '\r']*
let var = [^ ' ' '\t' '\n' '\r' '(' ')' '[' ']']+
rule token = parse
  white { token lexbuf }
| "read" { READ }
| "let" { LET }
| '(' { LP }
| ')' { RP }
| '[' { LB }
| ']' { RB }
| '+' { PLUS }
| '-' { NEGATE }
| ('-'?)['0'-'9']+ as lxm { INT (int_of_string lxm) }
| var as lxm { VAR (lxm) }
| eof { EOF }
| _ as lxm { raise (BadInput (Printf.sprintf "Illegal character %c" lxm)) }

