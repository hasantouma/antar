type expr = Ast.expr

type program =
  { info : bool
  ; e : expr
  }

let name = "R1"

let make_prog (expr : expr) : program = { info = false; e = expr }

let randp = Generator.randp

let interp = Interp.interp

let pp = Pp.pp

let parse (lexbuf : Lexing.lexbuf) : program = make_prog (Parse.main Lex.token lexbuf)

type vertex = Viz.vertex

type edge = Viz.edge

let node_style_of_expr = Viz.node_style_of_expr

let graph_of_expr = Viz.graph_of_expr
