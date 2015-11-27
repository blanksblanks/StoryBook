open Ast
open Sast

type symbol_table = {
  parent : symbol_table option;
  functions: Sast.function_decl list;
  variables : Sast.variable_decl list;
}

type translation_environment = {
  scope : symbol_table; (* symbol table for vars *)
  return_type : Sast.data_type; (* Function’s return type *)
}

(* Find Function *)
let rec find_function (scope: symbol_table) name =
	try
	   List.find(fun f -> f.fname = name) scope.functions
	with Not_found ->
	match scope.parent with
		Some(parent) -> find_function parent name
	| _ -> raise (Failure("function '" ^ name ^ "' not found"))

(* Find Variable *)
let rec find_variable (scope : symbol_table) name =
  try
    List.find (fun v -> v.vname = name) scope.variables
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _ -> raise (Failure("variable not found"))

(* Are types valid for this operation? *)
let analyze_op (scope: symbol_table) op t1 t2 = match op with
Add -> 
	if (t1 <> Sast.Boolean || t2 <> Sast.Boolean) then 
		if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("cannot use + on a tof and number"))
		else Sast.Number
		
	else Sast.Boolean
| _ -> Sast.Boolean

let convert_data_type old_type = match old_type with
  |Ast.Number -> Sast.Number
  |Ast.Boolean -> Sast.Boolean
  |Ast.String -> Sast.String
  |Ast.Char -> Sast.Char
  |Ast.Object(v) -> Sast.String


(* compare parameter types *)
let rec compare_p_types formalVars actualExprs = match formalVars with
    [] -> true
    |x::tail -> true
(* Expression Environment *)
let rec analyze_expr env = function

      (* Simple evaluation of primitives *)
      Ast.LitNum(v) -> Sast.LitNum(v), Sast.Number
    | Ast.LitChar(v) -> Sast.LitChar(v), Sast.Char
    | Ast.LitBool(v) -> Sast.LitBool(v), Sast.Boolean
    | Ast.LitString(v) -> Sast.LitString(v), Sast.String
    | Ast.Id(vname) ->
      let vdecl = try
	    find_variable env.scope vname (* locate a variable by name *)
      with Not_found ->
        raise (Failure("undeclared identifier " ^ vname))
      in Sast.Id(vdecl), vdecl.vtype (* return type *)

    | Ast.Binop(e1, op, e2) ->
	  let e1 = analyze_expr env e1 (* Check left and right children *)
	  and e2 = analyze_expr env e2 in
	  let _, t1 = e1 (* Get the type of each child *)
	  and _, t2 = e2 in let valid = analyze_op env op t1 t2 in
	  if valid = false then raise (Failure("invalid operation:"))
      else Sast.Binop(e1, op, e2), Sast.Number (* Success: result is int *)

    | Ast.FCall(fname, params) ->
      let actual_p_typed = List.map (fun e -> analyze_expr env e) params in
      let fdecl = try
        find_function env.scope fname
      with Not_found -> raise (Failure("function '" ^ fname ^ "' not found"))
      in let formal_p_list = fdecl.fformals in

      if (compare_p_types formal_p_list actual_p_typed) = true then Sast.FCall(fdecl, actual_p_typed), Sast.Number
      else raise (Failure("invalid parameters to function"))

    | _ -> Sast.LitString(""), Sast.String


let rec analyze_stmt env = function
  Ast.Expr(e) -> Sast.Expression(analyze_expr env e) (* expression *)
  (* If statement: verify the predicate is integer *)
  | Ast.If(e, s1, s2) ->
      let sastexpr = analyze_expr env e in (* Check the predicate *)
      let (_, typ) = sastexpr in
      if typ = Sast.Boolean then
      	Sast.If(sastexpr, analyze_stmt env s1, analyze_stmt env s2) (* Check then, else *)
      else raise(Failure("invalid if condition"))
  | _ -> Sast.Expression(Sast.LitString(""), Sast.String)

let library_funcs = [
  {
    (* Say is always going to evaluate to a String. Even if there are numbers as formals (e.g. say(1+1). it'll evaluate the 1+1 and print out the result as a String *)
    fname = "say";
    fformals = [{vtype = (Sast.String);
                 vname = "str";
                 vexpr = (Sast.Noexpr, Sast.String) (*will have to make void*)
                }];
    freturn = Sast.Number;
    funcbody = [Sast.Expression(Sast.LitString(""), Sast.String)]; 	   
  }
]

(* looks at list of function types in the s/ast *)
let analyze_func (fun_dcl : Ast.func_decl) env : Sast.function_decl =
  let name = fun_dcl.fname
  (*and old_formals = fun_dcl.fformals *)
  and old_ret_type = fun_dcl.freturn
  and old_body = fun_dcl.fbody in (*?*)
  let body = List.map (fun st -> analyze_stmt env st) old_body in (* stmt = analyze_stmt, just going through body and checking each statement *)
  let formals = [] in
  let ret_type = convert_data_type old_ret_type in
   {fname = name; fformals = formals; freturn = ret_type; funcbody= body}


(* the main of the program, goes through the scope, stores list of all declared lists and variables *)
let analyze_semantics prgm: Sast.program =
  let prgm_scope = {parent = None; functions = library_funcs; variables = []} in (* parent scope is the starting out scope *)
  let env = {scope = prgm_scope; return_type = Sast.Number} in (* main is always returning a number; might have to change to return type void; return type of the current function that you're in, so environment changes for every function *)
  let (_, func_decls) = prgm  in (* the anything supposed to be the character declrations. *)
  let new_func_decls = List.map (fun f -> analyze_func f env)func_decls in (* checks every single func in the Sast using analyze_func,  *)

  ([], List.append new_func_decls library_funcs)



















