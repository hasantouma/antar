
%token <int> INT
%token READ PROGRAM LP RP PLUS NEGATE WILDCARD EOF
%start expr_start
%type <Ast.expr> expr_start
%%
expr_start:
| INT { `EInt $1 }
| LP READ RP { `ERead }
| LP NEGATE expr_start RP { `ENegate $3 }
| LP PLUS expr_start expr_start RP { `EAdd($3, $4) }
| LP expr_start RP { $2 }
;

