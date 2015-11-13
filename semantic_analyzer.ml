open Ast
open Sast

type translation_environment = {
	scope : symbol_table; (* symbol table for vars *)
	found_plot: bool;
}

type symbol_table = {
	parent : symbol_table option;
	variables : (string * var_decl * var_types) list;
	functions : function_decl list;
	objects : class_decl list;
  actions : action_decl list;
  found_plot: bool;
}

let print_func = {
  freturn = Sast.Number;
  fname = "say";
  fformals = [];
  flocals = [];
  fbody = [];
}

let find_func( l : function_decl list) f =
  List.find(fun c -> c.fname = f)

let rec check_id (scope : symbol_table) id =
	try
		let (_, decl, t) = List.find(fun (n, _, _) -> n = id ) scope.variables in
		decl, t
	with Not_found -> match scope.parent with
		Some(parent) -> check_id parent id
		| _ -> raise Not_found

let rec check_expr (scope : symbol_table) (expr : Ast.expr) = match expr with
 	Noexpr -> Sast.Noexpr, Void
 	| Id(str) ->
 		(try
 			let (decl, t) = check_id scope str in Sast.Id(decl), t
 		with Not_found -> raise (Failure ("Id named " ^ str ^ " not found")))
 	| Integer_literal(i) -> Sast.Lit_int(i), Sast.Number
 	| String_literal(str) -> Sast.Lit_string(str), Sast.String
 	| Boolean_literal(b) -> Sast.Lit_bool(b), Sast.Boolean
  | Char_literal(c) -> Sast.Lit_char(c), Sast.Char
 	| Assign(_, _) as a -> check_assign scope a
 	| Call(_, _) as c -> check_call scope c
  and check_fcall (scope : symbol_table) c = match c with
    Ast.FCall(id, expr_list) ->
      (try
        let f = find_func scope.functions id in
        let exprs = List.fold_left2 (
          (* checked actual param, formal params, actual params *)
						fun a b c ->
							let (_, t) = b in
							let expr = check_expr scope c in
							let (_, t2) = expr in
							if t <> t2
							then raise (Failure "Wrong types in function  params")
							else expr :: a
					) [] f.fformals expr_list in
        Sast.Call(f, exprs), f.freturn
			with Not_found ->
				if id = "say" then match expr_list with
					| hd :: [] -> let expr = check_expr scope hd in
						let (_, t) = expr in
						if (t = Sast.String || t = Sast.Number) then Sast.Call(print_func, [expr]), Sast.Number else raise (Failure "Print takes only type string or int")
					| _ -> raise (Failure "Print only takes one argument")
        )
      | _ -> raise(Failure "Not a call")
