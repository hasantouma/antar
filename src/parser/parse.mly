
%token <int> INT
%token <string> VAR
%token PROGRAM READ LET
%token LP RP LB RB
%token NEGATE PLUS MULT WILDCARD
%token EOF
%start main
%type <R1.Ast.r1> main
%%
main:
  expr EOF { $1 }
expr:
| INT { `EInt $1 }
| VAR { `EVar $1 }
| LP READ RP { `ERead }
| LP NEGATE expr RP { `ENegate $3 }
| LP PLUS expr expr RP { `EAdd($3, $4) }
| LP MULT expr expr RP { `EMult($3, $4) }
| LP LET LP LB VAR expr RB RP expr RP { `ELet ($5, $6, $9) }
;

