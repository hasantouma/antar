module type Rlang = sig
  type expr

  type vertex = int * expr

  type edge = vertex * vertex

  val node_style_of_expr : expr -> Graph.Graphviz.DotAttributes.vertex list

  val graph_of_expr : expr -> vertex list * edge list
end
