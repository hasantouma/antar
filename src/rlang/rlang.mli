module type Rlang = sig
  type expr

  type program =
    { info : bool
    ; e : expr
    }

  val make_prog : expr -> program

  val interp : ?env:(string * int) list -> ?inputs:int list -> expr -> int

  val pp : expr -> string

  val parse : Lexing.lexbuf -> program

  val randp : ?vars:string list -> int -> expr

  type vertex = int * expr

  type edge = vertex * vertex

  val node_style_of_expr : expr -> Graph.Graphviz.DotAttributes.vertex list

  val graph_of_expr : expr -> vertex list * edge list
end
