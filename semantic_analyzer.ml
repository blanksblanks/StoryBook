open Ast
open Sast

type translation_environment = {
	scope : symbol_table; (* symbol table for vars *)
	found_plot: bool;
}

type symbol_table = {
	parent : symbol_table option;
	variables : (string * var_decl * var_type) list;
	functions : function_decl list;
	classes : class_decl list;
  actions : action_decl list;
  return: bool;
}

let print_func = {
  freturn = Sast.Number;
  fname = "say";
  fformals = [];
  flocals = [];
  fbody = [];
}

let find_func(l : function_decl list) f =
  List.find(fun k -> k.fname = f)

let find_class(c : class_decl list) class =
	List.find(fun k -> k.cname = class) c

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

let rec check_var_type (scope : symbol_table) (v : Ast.var_type) = match v with
	Ast.Number -> Sast.Number
	| Ast.String -> Sast.String
	| Ast.Boolean -> Sast.Boolean
	| Ast.Char -> Sast.Char
	| Ast.Object(id) ->
		(try
			let c = find_class scope.classes id in
			Sast.Object(c)
		with Not_found -> raise (Failure ("Character " ^ id ^ " not found.")))

let rec check_func_stmt (scope : symbol_table) (stmt_lst : Sast.stmt list) (ftype : Sast.var_types) =
	List.iter (
		fun s -> match s with
		Sast.Block (stmt_list) ->
			check_func_stmt scope stmt_list ftype
		| Sast.Return(e) ->
			let (_, t) = e in
			if t <> ftype then raise (Failure "Return statement returns the wrong type.") else ()
		| Sast.If(_, s1, s2) ->
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype
		| Sast.For(_, _, _, s) ->
			check_func_stmt scope [s] ftype
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype
		| _ -> ()
	) stmt_lst

let check_func_decl (env : translation_environment) (f : Ast.func_decl) =
	let scope' = { env.scope with parent = Some(env.scope); variables = []; return_found = false } in
	let rettype = check_var_type env.scope f.freturn in
	let formals = List.fold_left (
		fun k f -> match f with
		Ast.Parameter(rettype, strng) -> (* Parameter of var_type * string *)
			let rettype = check_var_type scope' rettype in
			scope'.variables <- (strng, Sast.Variable(rettype, strng), rettype) :: scope'.variables; (Sast.Variable(rettype, strng), rettype) :: k
	) [] f.fformals in
	let statements = process_func_stmt scope' f.body rettype in
	(* if scope'.return_found then; we may not need return_found if we do not have voids *)
	let f = { fname = f.fname; fformals = formals; freturn = rettype; fbody = statements } in
	env.scope.functions <- f :: env.scope.functions; (* throw away scope of function *) f
	else
		raise (Failure ("No return for Chapter " ^ f.fname ^ " when return expected.")))

let process_func_stmt (scope : symbol_table) (stmt_lst : Ast.stmt list) (ftype : Sast.var_types) =
	List.fold_left (
		fun a s -> let stmt = check_stmt scope s in
		match stmt with
		Sast.Block (stmt_list) ->
			check_func_stmt scope stmt_list ftype; stmt :: a
		| Sast.Return(e) ->
			let (_, t) = e in
			if t <> ftype then raise (Failure "Return statement returns the wrong type.") else
			scope.return_found <- true; stmt :: a
		| Sast.If(_, s1, s2) ->
			check_func_stmt scope [s1] ftype; check_func_stmt scope [s2] ftype; stmt :: a
		| Sast.For(_, _, _, s) ->
			check_func_stmt scope [s] ftype; stmt :: a
		| Sast.While(_, s) ->
			check_func_stmt scope [s] ftype; stmt :: a
		| _ -> stmt :: a
	) [] stmt_lst

let process_func_decl (env : translation_environment) (f : Ast.func_decl) =
	try
		let _ = find_func env.scope.functions f.fname in
			raise (Failure ("Function already declared with name " ^ f.fname))
	with Not_found ->
		if f.fname = "say" then raise (Failure "A function cannot be named" ^ f.fname)
		else
			if f.fname = "plot" then
				(
					if f.freturn <> Number || (List.length f.fformals) <> 0 then
					raise (Failure "plot function must return numbers and cannot contain parameters")
					else
						let func = check_func_decl env f in
						env.found_plot <- true; func
					)
			else
				check_func_decl env f

(* let process_class_decl (env : translation_environment) (c : Ast.class_decl) =
	try
		let _ = find_class env.scope.classes c.cname in
			raise (Failure ("Class already declared with name " ^ c.cname))
	with Not_found ->
		let scope' = { env.scope with parent = Some(env.scope); variables = [] } in
		let formals =
		let instvars = List.fold_left ( fun a v -> process_var_decl scope' v :: a ) [] c.variable_decls in
		let actions =
		(* cname, cformals, cinstvars, cactions; param_decl, var decl, action_decl *)
		env.scope.classes <- c :: env.scope.classes; c *)

let check_program (p : Ast.program) =
	let s = { parent = None; classes = []; functions = []; return_plot = false } in
	let env = { scope = s; found_plot = false } in
	let (classes, funcs) = p in
	(* let classes =
		List.fold_left (
			fun a s -> process_class_decl env s :: a
		) [] classes in *)
	let classes = [] in
	let funcs =
		List.fold_left (
			fun a f -> process_func_decl env f :: a
		) [] (List.rev funcs) in
  (if env.found_plot then classes, funcs else (raise (Failure "No Plot defined.")))
