module Make_viz (L : Rlang.Rlang) = struct
  module Node = struct
    type t = int * L.expr

    (* This will preserve the order of nodes, which will preserve the order of operation in the graph *)
    let compare (id1, _) (id2, _) = compare id1 id2

    let hash = Hashtbl.hash

    let equal = ( = )
  end

  module Edge = struct
    type t = string

    let compare = compare

    let equal = ( = )

    let default = ""
  end

  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Node) (Edge)

  (* Graphviz.DotAttributes : http://ocamlgraph.lri.fr/doc/Graphviz.DotAttributes.html#TYPEgraph *)
  module Dot = Graph.Graphviz.Dot (struct
    include G (* use the graph module from above *)

    let edge_attributes (_, e, _) = [ `Label e; `Color 4711 ]

    let default_edge_attributes _ = []

    let get_subgraph _ = None

    let vertex_attributes (_, e) = L.node_style_of_expr e

    let vertex_name (id, _) = string_of_int id

    let default_vertex_attributes _ = []

    let graph_attributes _ = []
  end)

  let generate_graph expr =
    let g = G.empty in
    let vs, es = L.graph_of_expr expr in
    let ns = List.map (fun v -> (v, G.V.create v)) vs in
    let get e = List.assoc e ns in
    let g = List.fold_left G.add_vertex g (List.map snd ns) in
    let g = List.fold_left (fun acc (v1, v2) -> G.add_edge acc (get v1) (get v2)) g es in
    g

  let write_expr_to_graphviz (expr : L.expr) : unit =
    let name = "mygraph.dot" in
    let g = generate_graph expr in
    let file = open_out_bin name in
    Dot.output_graph file g;
    let _ = Sys.command (Printf.sprintf "dot %s -Tpng -o mygraph.png && rm %s" name name) in
    print_endline "Success! Open 'mygraph.png' to see your visualized expr."
end
