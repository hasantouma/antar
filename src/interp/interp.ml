open Lex_parse.Ast

exception Non_empty_list of string

let interp_all (f : expr -> int list -> (int * int list)) (expr : expr) (lst : int list) : (int * int list) =
  match expr with
  | #R0.Ast.expr as y -> R0.Interp.interp f y lst

let interp (expr : expr) (lst : int list) : int =
	let rec interp (expr : expr) (lst : int list) : (int * int list) =
		interp_all interp expr lst
	in
	let (v, lst') = interp expr lst in
	match lst' with
	| [] -> v
	| _ -> raise (Non_empty_list "List is not empty!")

