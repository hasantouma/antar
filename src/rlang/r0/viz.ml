type vertex = int * Ast.expr
type edge = vertex * vertex

let label_of_expr_type expr lst =
  let label =
    match expr with
    | `EInt n -> string_of_int n
    | `ERead -> "(read)"
    | `ENegate _ -> "-"
    | `EAdd _ -> "+"
  in
  `Label label :: lst

let shape_of_expr_type expr lst =
  let shape =
    match expr with
    | `EInt _ | `ERead -> `Box
    | _ -> `Oval
  in
  `Shape shape :: lst

let style_of_expr_type expr lst =
  match expr with
  | `EInt _ | `ERead -> `Style `Filled :: lst
  | _ -> lst

let node_style_of_expr (expr : Ast.expr) : Graph.Graphviz.DotAttributes.vertex list =
  label_of_expr_type expr [] |> shape_of_expr_type expr |> style_of_expr_type expr

let fresh =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    !counter

let[@warning "-8"] rec graph_of_expr (expr : Ast.expr) : vertex list * edge list =
  let id = fresh () in
  match expr with
  | `EInt _ | `ERead -> ([ (id, expr) ], [])
  | `ENegate e ->
    let v :: vs, es = graph_of_expr e in
    let from = (id, expr) in
    let vertices = from :: v :: vs in
    let edges = (from, v) :: es in
    (vertices, edges)
  | `EAdd (l, r) ->
    let lv :: lvs, les = graph_of_expr l in
    let rv :: rvs, res = graph_of_expr r in
    let from = (id, expr) in
    let vertices = (from :: lv :: rv :: lvs) @ rvs in
    let edges = ((from, lv) :: (from, rv) :: les) @ res in
    (vertices, edges)
