
%token <int> INT
%token <string> VAR
%token READ LET
%token LP RP LB RB
%token NEGATE PLUS
%token EOF
%start main
%type <R1.Ast.r1> main
%%
main:
  expr EOF { $1 }
expr:
| n = INT { `EInt n }
| v = VAR { `EVar v }
| LP; READ; RP { `ERead }
| LP; NEGATE; e = expr; RP { `ENegate e }
| LP; PLUS; l = expr; r = expr; RP { `EAdd (l, r) }
| LP; LET; LP; LB; x = VAR; ex = expr; RB; RP; eb = expr; RP { `ELet (x, ex, eb) }
;

