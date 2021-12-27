include Ast

let name = "R1"
let make_rprog (expr : expr) : rprogram = { info = false; e = expr }
let randp = Generator.randp
let generate_input_for_randp = Generator.generate_input_for_randp
let interp = Interp.interp
let optimize = Interp.optimize
let pp = Pp.pp
let parse (lexbuf : Lexing.lexbuf) : rprogram = make_rprog (Parse.main Lex.token lexbuf)

type vertex = Viz.vertex
type edge = Viz.edge

let node_style_of_expr = Viz.node_style_of_expr
let graph_of_expr = Viz.graph_of_expr
