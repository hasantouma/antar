exception SyntaxError of string

type rvar = string

type expr = Ast.expr =
  | EInt of int
  | ERead
  | ENegate of expr
  | EAdd of expr * expr
  | EVar of rvar
  | ELet of rvar * expr * expr

type rprogram =
  { info : bool
  ; e : expr
  }

val name : string
val make_rprog : expr -> rprogram
val interp : ?env:(string * int) list -> ?inputs:int list -> rprogram -> int
val optimize : ?env:(string * expr) list -> rprogram -> rprogram
val pp : rprogram -> string
val parse : Lexing.lexbuf -> rprogram
val randp : ?vars:string list -> int -> rprogram
val generate_input_for_randp : rprogram -> int list

type vertex = int * expr
type edge = vertex * vertex

val node_style_of_expr : expr -> Graph.Graphviz.DotAttributes.vertex list
val graph_of_expr : expr -> vertex list * edge list
