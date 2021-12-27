type var = string

type expr = Ast.expr =
  | EInt of int
  | ERead
  | ENegate of expr
  | EAdd of expr * expr
  | EVar of var
  | ELet of var * expr * expr

type rprogram =
  { info : bool
  ; e : expr
  }

val name : string
val make_prog : expr -> rprogram
val interp : ?env:(string * int) list -> ?inputs:int list -> expr -> int
val optimize : ?env:(string * expr) list -> expr -> expr
val pp : expr -> string
val parse : Lexing.lexbuf -> rprogram
val randp : ?vars:string list -> int -> expr
val generate_input_for_randp : expr -> int list

type vertex = int * expr
type edge = vertex * vertex

val node_style_of_expr : expr -> Graph.Graphviz.DotAttributes.vertex list
val graph_of_expr : expr -> vertex list * edge list
