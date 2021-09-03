{
 open Exceptions
 open Parse
 exception Eof
}
rule token = parse
  [' ' '\t' '\n' '\r']* { token lexbuf }
| "program" { PROGRAM }
| "read" { READ }
| "let" { LET }
| '(' { LP }
| ')' { RP }
| '[' { LB }
| ']' { RB }
| '+' { PLUS }
| '-' { NEGATE }
| '_' { WILDCARD }
| ['0'-'9']+ as lxm { INT (int_of_string lxm) }
| [^ ' ' '\t' '\n' '\r' '(' ')' '[' ']']+ as lxm { VAR (lxm) }
| eof { EOF }
| _ as lxm { raise (BadInput (Printf.sprintf "Illegal character %c" lxm)) }

