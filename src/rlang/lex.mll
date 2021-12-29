{
 open Exceptions
 open Parse
 exception Eof
}

let white = [' ' '\t' '\n' '\r']*
let digits = ('-'?)['0'-'9']+
let var = [^ ' ' '\t' '\n' '\r' '(' ')' '[' ']']+
rule token = parse
  white { token lexbuf }
| "read" { READ }
| "let" { LET }
| ';' [^ '\n']* '\n' { token lexbuf } (* skip ;'d lines as comments *)
| '(' { LP }
| ')' { RP }
| '[' { LB }
| ']' { RB }
| '+' { PLUS }
| '-' { NEGATE }
| digits as lxm { INT (int_of_string lxm) }
| var as lxm { VAR (lxm) }
| eof { EOF }
| _ as lxm { raise (BadInput (Printf.sprintf "Illegal character %c" lxm)) }

