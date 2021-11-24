
%token <int> INT
%token READ
%token LP RP
%token NEGATE PLUS
%token EOF
%start main
%type <Ast.expr> main
%%
main:
  expr EOF { $1 }
expr:
| n = INT { `EInt n }
| LP; READ; RP { `ERead }
| LP; NEGATE; e = expr; RP { `ENegate e }
| LP; PLUS; l = expr; r = expr; RP { `EAdd (l, r) }
;

