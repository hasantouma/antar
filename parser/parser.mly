
%token <int> INT
%token READ PROGRAM LP RP PLUS NEGATE WILDCARD EOF
%start prog
%type <Ast.expr> expr
%type <Ast.program> prog
%%
prog:
 LP PROGRAM WILDCARD expr RP EOF { { info=false; e=$4 } }
;
expr:
| INT { `EInt $1 }
| LP READ RP { `ERead }
| LP NEGATE expr RP { `ENegate $3 }
| LP PLUS expr expr RP { `EAdd($3, $4) }
| LP expr RP { $2 }
;

