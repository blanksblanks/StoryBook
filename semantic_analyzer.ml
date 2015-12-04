open Ast
open Sast

type symbol_table = {
  parent : symbol_table option;
  mutable functions: Sast.function_decl list;
  mutable variables : Sast.variable_decl list;
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

let find_plot (l : Sast.function_decl list) =
      try
         List.find(fun f -> f.fname = "plot") l
      with Not_found -> raise (Failure("No plot found"))


(* Find Variable *)
let rec find_variable (scope : symbol_table) name =
  try
    List.find(fun v -> v.vname = name) scope.variables
  with Not_found ->
    match scope.parent with
      Some(parent) -> find_variable parent name
    | _ -> raise (Failure("variable not found"))

(* Checking types for binop; takes the op anad the two types to do checking *)
let analyze_binop (scope: symbol_table) op t1 t2 = match op with
  Add -> 
  	if (t1 == Sast.String || t2 == Sast.String) then Sast.String
  	else if (t1 == Sast.Number || t2 == Sast.Number) then 
  		if (t1 == Sast.Boolean || t2 == Sast.Boolean) then raise (Failure("Invalid use of + for operands' types"))
  		else if (t1 == Sast.Number && t2 == Sast.Number) then Sast.Number
  		else Sast.String
  	else if (t1 == Sast.Char || t2 == Sast.Char) then 
  		if (t1 == Sast.Boolean || t2 == Sast.Boolean) then raise (Failure("Invalid use of + for operands' types"))
  		else Sast.String
  	else raise (Failure("Invalid use of + for operands' types"))			
  	
  | Sub -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of - for operands' types")) else Sast.Number
  | Mult -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of * for operands' types")) else Sast.Number
  | Div -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of / for operands' types")) else Sast.Number
  | Equal -> 	if (t1 <> t2) then raise (Failure("Invalid use of = for operands' types")) else Sast.Boolean 
  | Neq -> 	if (t1 <> t2) then raise (Failure("Invalid use of not= for operands' types")) else Sast.Boolean 
  | Less ->  	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of < for operands' types")) else Sast.Number
  | Leq -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of <= for operands' types")) else Sast.Number
  | Greater ->	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of > for operands' types")) else Sast.Number
  | Geq -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of >= for operands' types")) else Sast.Number
  | Mod -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of % for operands' types")) else Sast.Number
  | OR -> 	if (t1 <> Sast.Boolean || t2 <> Sast.Boolean) then raise (Failure("Invalid use of or for operands' types")) else Sast.Boolean
  | AND -> 	if (t1 <> Sast.Boolean || t2 <> Sast.Boolean) then raise (Failure("Invalid use of and for operands' types")) else Sast.Boolean
  | NOT -> 	raise (Failure("Invalid use of ! for two operands"))
  (*| _ ->		raise (Failure("Invalid binary operator")) if this line uncomment, get a case unused warning *)

let analyze_unop (scope: symbol_table) op t1 = match op with
NOT -> 		if (t1 <> Sast.Boolean) then raise (Failure("Invalid use of ! for operand type")) else Sast.Boolean
| _ -> 		raise (Failure("Invalid unary operator")) 

let convert_data_type old_type = match old_type with
  | Ast.Void -> Sast.Void 
  | Ast.Number -> Sast.Number
  | Ast.Boolean -> Sast.Boolean
  | Ast.String -> Sast.String
  | Ast.Char -> Sast.Char
  | Ast.Object(v) -> Sast.String


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
    | Ast.Assign(vname, expr) ->
        let vdecl = try
          find_variable env.scope vname 
        with Not_found ->
          raise (Failure("undeclared identifier " ^ vname))  
        in let (e, expr_typ) = analyze_expr env expr 
        in if vdecl.vtype <> expr_typ then raise(Failure("Expression does not match variable type")) 
        else 
          let _ = vdecl.vexpr <- (e, expr_typ) in (* change variable expr in symbol_table *)
          Sast.Assign(vname, (e, expr_typ)), expr_typ 
    | Ast.Binop(e1, op, e2) ->
	  let e1 = analyze_expr env e1 (* Check left and right children *)
	  and e2 = analyze_expr env e2 in
	  let _, t1 = e1 (* Get the type of each child *)
	  and _, t2 = e2 in (*let valid = *)
	  let validbinop = try
		analyze_binop env.scope op t1 t2
	  with Not_found -> raise (Failure("Invalid binary operator"))
	  in Sast.Binop(e1, op, e2), validbinop (* Success: result is int *)

    | Ast.Unop(op, e1) ->
	  let e1 = analyze_expr env e1 in
	  let _, t1 = e1 in 
	  let validunop = try
		analyze_unop env.scope op t1
	  with Not_found -> raise (Failure("Invalid unary operator"))
	  in Sast.Unop(op, e1), validunop

    | Ast.FCall(fname, params) ->
      let actual_p_typed = List.map (fun e -> analyze_expr env e) params in
      let fdecl = try
        find_function env.scope fname
      with Not_found -> raise (Failure("function '" ^ fname ^ "' not found"))
      in let formal_p_list = fdecl.fformals in

      if (compare_p_types formal_p_list actual_p_typed) = true then Sast.FCall(fdecl, actual_p_typed), Sast.Number
      else raise (Failure("invalid parameters to function"))

    | _ -> Sast.LitString(""), Sast.String

(* convert ast.var_decl to sast.variable_decl*)
let check_var_decl (env: translation_environment) (var: Ast.var_decl) =
  let typ = convert_data_type var.vtype in
    let (e, expr_typ) = analyze_expr env var.vexpr in
          if typ <> expr_typ then raise(Failure("Variable assignment does not match variable type"))
          else { vtype = typ; vname = var.vname; vexpr = (e, expr_typ) }  

let rec analyze_stmt env = function
  Ast.Expr(e) -> Sast.Expression(analyze_expr env e) (* expression *)
  | Ast.VarDecl(var_decl) ->
          if List.exists (fun x -> x.vname = var_decl.vname) env.scope.variables then
            raise(Failure("Variable already declared in this scope")) 
          else 
            let sast_var = check_var_decl env var_decl in
            let _ = env.scope.variables <- sast_var :: env.scope.variables in
            Sast.VarDecl(sast_var);  
  (* If statement: verify the predicate is integer *)
  | Ast.If(e, s1, s2) ->
      let sastexpr = analyze_expr env e in (* Check the predicate *)
      let (_, typ) = sastexpr in
      if typ = Sast.Boolean then
      	Sast.If(sastexpr, analyze_stmt env s1, analyze_stmt env s2) (* Check then, else *)
      else raise(Failure("invalid if condition"))
  | Ast.Return(e) -> let sastexpr = analyze_expr env e in Sast.Return(sastexpr)
  | Ast.For(e1, e2, e3, s) ->
      let sastexpr1 = analyze_expr env e1 in 
      let sastexpr2 = analyze_expr env e2 in
      let (_, typ) =  sastexpr2 in
      if typ <> Sast.Boolean then
        raise(Failure("For loop must have boolean condition"))
      else let sastexpr3 = analyze_expr env e3 in
      let s = analyze_stmt env s in
      Sast.For(sastexpr1, sastexpr2, sastexpr3, s)
  | Ast.While(e, s) ->
      let sastexpr = analyze_expr env e in
      let (_, typ) = sastexpr in
      if typ <> Sast.Boolean then
        raise(Failure("While condition must be a boolean expression"))
      else let s = analyze_stmt env s in
      Sast.While(sastexpr, s)
  | _ -> Sast.Expression(Sast.LitString(""), Sast.String)

let library_funcs = [
  {

    fname = "say";
    fformals = [{vtype = (Sast.String);
                 vname = "str";
                 vexpr = (Sast.Noexpr, Sast.String) (*will have to make void*)
                }];
    freturn = Sast.Number;
    funcbody = [Sast.Expression(Sast.LitString(""), Sast.String)]; 	   
  }
]

let check_ret (expTyp: Sast.data_type) (env: translation_environment) (f: Sast.statement) = match f with
  Sast.Return(e) -> let (_, typ) = e in 
    if expTyp = typ  then true 
    else if expTyp = Sast.Void then raise (Failure("Void function cannot return a value")) 
    else raise (Failure ("Incorrect return type"))(* true if correct type, false in wrong return type *)
  | _ -> false

let find_return (body_l : Sast.statement list) (env: translation_environment) (expTyp: Sast.data_type) =
  try
     List.find(check_ret expTyp env) body_l 
  with Not_found -> if expTyp <> Sast.Void then raise (Failure("No return found")) else Expression(Noexpr, Void)


let analyze_func (fun_dcl : Ast.func_decl) env : Sast.function_decl =
  let name = fun_dcl.fname in
    if name = "say" then raise(Failure("Cannot use library function name: " ^ name))
  (*and old_formals = fun_dcl.fformals *)
  else let old_ret_type = fun_dcl.freturn 
  and old_body = fun_dcl.fbody in (*?*)
  let body = List.map (fun st -> analyze_stmt env st) old_body in 
  let formals = [] in
  let ret_type = convert_data_type old_ret_type in
  let _ = find_return body env ret_type in
   {fname = name; fformals = formals; freturn = ret_type; funcbody= body}



let analyze_semantics prgm: Sast.program =
  let prgm_scope = {parent = None; functions = library_funcs; variables = []} in 
  let env = {scope = prgm_scope; return_type = Sast.Number} in 
  let (_, func_decls) = prgm  in 
  let new_func_decls = List.map (fun f -> analyze_func f env)func_decls in 

  (* Search for plot *)
  let _  = try
        find_plot new_func_decls
      with Not_found -> raise (Failure("No plot was found.")) in
  ([], List.append new_func_decls library_funcs)

