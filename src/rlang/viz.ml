open Ast

type vertex = int * expr
type edge = vertex * vertex

let label_of_expr_type expr lst =
  let label =
    match expr with
    | EInt n -> string_of_int n
    | ERead -> "(read)"
    | ENegate _ -> "-"
    | EAdd _ -> "+"
    | EVar v -> v
    | ELet _ -> "let"
  in
  `Label label :: lst

let shape_of_expr_type expr lst =
  let shape =
    match expr with
    | EInt _ | ERead | EVar _ -> `Box
    | _ -> `Oval
  in
  `Shape shape :: lst

let style_of_expr_type expr lst =
  match expr with
  | EInt _ | ERead | EVar _ -> `Style `Filled :: lst
  | _ -> lst

let node_style_of_expr (expr : expr) : Graph.Graphviz.DotAttributes.vertex list =
  label_of_expr_type expr [] |> shape_of_expr_type expr |> style_of_expr_type expr

let fresh =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    !counter

let[@warning "-8"] rec graph_of_expr (expr : expr) : vertex list * edge list =
  let id = fresh () in
  match expr with
  | EInt _ | ERead | EVar _ -> ([ (id, expr) ], [])
  | ENegate e ->
    let v :: vs, es = graph_of_expr e in
    let from = (id, expr) in
    let vertices = from :: v :: vs in
    let edges = (from, v) :: es in
    (vertices, edges)
  | EAdd (l, r) ->
    let lv :: lvs, les = graph_of_expr l in
    let rv :: rvs, res = graph_of_expr r in
    let from = (id, expr) in
    let vertices = (from :: lv :: rv :: lvs) @ rvs in
    let edges = ((from, lv) :: (from, rv) :: les) @ res in
    (vertices, edges)
  | ELet (x, ex, eb) ->
    let vv = (fresh (), EVar x) in
    let xv :: xvs, xes = graph_of_expr ex in
    let bv :: bvs, bes = graph_of_expr eb in
    let from = (id, expr) in
    let vertices = (from :: vv :: xv :: bv :: xvs) @ bvs in
    let edges = ((from, vv) :: (vv, xv) :: (from, bv) :: xes) @ bes in
    (vertices, edges)
