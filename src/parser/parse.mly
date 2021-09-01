
%token <int> INT
%token READ PROGRAM LP RP PLUS NEGATE WILDCARD EOF MULT
%start expr_start
%type <R1.Ast.expr> expr_start
%%
expr_start:
  expr EOF { $1 }
expr:
| INT { `EInt $1 }
| LP READ RP { `ERead }
| LP NEGATE expr RP { `ENegate $3 }
| LP PLUS expr expr RP { `EAdd($3, $4) }
| LP MULT expr expr RP { `EMult($3, $4) }
;

