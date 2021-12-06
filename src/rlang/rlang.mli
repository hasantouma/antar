module type Rlang = sig
  type expr

  type rprogram =
    { info : bool
    ; e : expr
    }

  val name : string

  val make_prog : expr -> rprogram

  val interp : ?env:(string * int) list -> ?inputs:int list -> expr -> int

  val pp : expr -> string

  val parse : Lexing.lexbuf -> rprogram

  val randp : ?vars:string list -> int -> expr

  type vertex = int * expr

  type edge = vertex * vertex

  val node_style_of_expr : expr -> Graph.Graphviz.DotAttributes.vertex list

  val graph_of_expr : expr -> vertex list * edge list
end