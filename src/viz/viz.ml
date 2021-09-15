open R1.Ast

module Node = struct
  type t = expr * int

  (* This will preserve the order of nodes, which will preserve the order of operation in the graph *)
  let compare (_, id1) (_, id2) = compare id1 id2

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

let label_of_expr_type expr lst =
  let label =
    match expr with
    | `EInt n -> string_of_int n
    | `ERead -> "(read)"
    | `ENegate _ -> "-"
    | `EAdd _ -> "+"
    | `EVar v -> v
    | `ELet _ -> "let"
  in
  `Label label :: lst

let shape_of_expr_type expr lst =
  let shape =
    match expr with
    | `EInt _ -> `Box
    | `ERead -> `Box
    | `ENegate _ -> `Oval
    | `EAdd _ -> `Oval
    | `EVar _ -> `Box
    | `ELet _ -> `Oval
  in
  `Shape shape :: lst

let style_of_expr_type expr lst =
  match expr with
  | `EInt _
  | `ERead
  | `EVar _ ->
    `Style `Filled :: lst
  | _ -> lst

let vertex_attr (expr : expr) : Graph.Graphviz.DotAttributes.vertex list =
  label_of_expr_type expr [] |> shape_of_expr_type expr |> style_of_expr_type expr

(* Graphviz.DotAttributes : http://ocamlgraph.lri.fr/doc/Graphviz.DotAttributes.html#TYPEgraph *)
module Dot = Graph.Graphviz.Dot (struct
  include G (* use the graph module from above *)

  let edge_attributes (_, e, _) = [ `Label e; `Color 4711 ]

  let default_edge_attributes _ = []

  let get_subgraph _ = None

  let vertex_attributes (e, _) = vertex_attr e

  let vertex_name (_, id) = string_of_int id

  let default_vertex_attributes _ = []

  let graph_attributes _ = []
end)

let fresh =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    !counter

let rec generate_graph (g : G.t) expr =
  let id = fresh () in
  match expr with
  | `EInt _
  | `ERead
  | `EVar _ ->
    (* Terminal Node *)
    let v = G.V.create (expr, id) in
    let g' = G.add_vertex g v in
    (g', v)
  | `ENegate e ->
    let negate_v = G.V.create (expr, id) in
    let g = G.add_vertex g negate_v in
    let g, v = generate_graph g e in
    let g = G.add_edge g negate_v v in
    (g, negate_v)
  | `EAdd (l, r) ->
    let add_v = G.V.create (expr, id) in
    let g = G.add_vertex g add_v in
    let g, lv = generate_graph g l in
    let g, rv = generate_graph g r in
    let g = G.add_edge g add_v lv in
    let g = G.add_edge g add_v rv in
    (g, add_v)
  | `ELet (x, ex, eb) ->
    let add_v = G.V.create (expr, id) in
    let add_x = G.V.create (`EVar x, fresh ()) in
    let g = G.add_vertex g add_v in
    let g, xv = generate_graph g ex in
    let g, xb = generate_graph g eb in
    let g = G.add_edge g add_v add_x in
    let g = G.add_edge g add_v xv in
    let g = G.add_edge g add_v xb in
    (g, add_v)

let g = G.empty

let write_expr_to_graphviz (program_opt : Ast.program option) : unit =
  let name = "mygraph.dot" in
  match program_opt with
  | Some p ->
    let g, _ = generate_graph g p.e in
    let file = open_out_bin name in
    Dot.output_graph file g;
    let _ = Sys.command (Printf.sprintf "dot %s -Tpng -o mygraph.png && rm %s" name name) in
    print_endline "Success! Open 'mygraph.png' to see your visualized expr."
  | None -> print_endline "Invalid expression. Can't visualize"
