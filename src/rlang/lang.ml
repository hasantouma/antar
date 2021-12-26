open Ast

let name = "R1"
let make_prog (expr : expr) : rprogram = { info = false; e = expr }
let randp = Generator.randp
let interp = Interp.interp
let pp = Pp.pp
let parse (lexbuf : Lexing.lexbuf) : rprogram = make_prog (Parse.main Lex.token lexbuf)

type vertex = Viz.vertex
type edge = Viz.edge

let node_style_of_expr = Viz.node_style_of_expr
let graph_of_expr = Viz.graph_of_expr
