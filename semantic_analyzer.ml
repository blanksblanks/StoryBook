open Ast
open Sast

(* keep track of amount of objects newed for memory management--freeing*)
let new_count = ref 0 
let increment_new_count() = new_count := !new_count + 1

type symbol_table = {
  parent : symbol_table option;
  mutable functions: Sast.function_decl list;
  mutable variables : Sast.variable_decl list;
  mutable characters: Sast.class_decl list;
}

type translation_environment = {
  scope : symbol_table; (* symbol table for vars *)
  return_type : Sast.data_type; (* Function’s return type *)
}

type func_wrapper = 
  Some of Sast.function_decl
  | None


(* Find Function *)
let rec find_function (scope: symbol_table) name =
	try
	   List.find(fun f -> f.fname = name) scope.functions
	with Not_found ->
	match scope.parent with
		Some(parent) -> find_function parent name
	| _ -> raise (Failure("function '" ^ name ^ "' not found"))


let rec is_func_name_already_used (scope: symbol_table) name : func_wrapper =
  try
     Some(List.find(fun f -> f.fname = name) scope.functions)
  with Not_found ->
  match scope.parent with
    Some(parent) -> is_func_name_already_used parent name
  | _ -> None



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
    | _ -> raise (Failure("variable not found" ^ name))

(* Find Function *)
let rec find_class_decl (scope: symbol_table) name =
  try
     List.find(fun c -> c.cname = name) scope.characters
  with Not_found ->
  match scope.parent with
    Some(parent) -> find_class_decl parent name
  | _ -> raise (Failure("function '" ^ name ^ "' not found"))


let rec find_class_var (scope: symbol_table) c_dec name =
  try List.find(fun v-> v.vname = name) c_dec.cinstvars
  with Not_found -> raise(Failure("invalid trait name" ^ name))

let get_class_decl_from_type (scope: symbol_table) ctype =
  match ctype with
  Sast.Object(typDecl) -> find_class_decl scope typDecl.cname
  | _ -> raise(Failure("not an object. can't access instance vars"))

let find_action_decl (actions : Sast.action_decl list) name =
  try
    List.find(fun a -> a.aname = name) actions
  with Not_found -> raise (Failure("variable not found" ^ name))

(* Checking types for binop; takes the op anad the two types to do checking *)
let analyze_binop (scope: symbol_table) op t1 t2 = match op with
  Add ->
  	if (t1 == Sast.String || t2 == Sast.String) then Sast.String
  	else if (t1 == Sast.Number || t2 == Sast.Number) then
  		if (t1 == Sast.Boolean || t2 == Sast.Boolean) then raise (Failure("Invalid use of + for operands' types"))
  		else if (t1 == Sast.Number && t2 == Sast.Number) then Sast.Number
      else if (t1 == Sast.Char || t2 == Sast.Char) then raise (Failure("Invalid use of + for operands' types"))
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
  | Less ->  	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of < for operands' types")) else Sast.Boolean
  | Leq -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of <= for operands' types")) else Sast.Boolean
  | Greater ->	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of > for operands' types")) else Sast.Boolean
  | Geq -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of >= for operands' types")) else Sast.Boolean
  | Mod -> 	if (t1 <> Sast.Number || t2 <> Sast.Number) then raise (Failure("Invalid use of % for operands' types")) else Sast.Number
  | OR -> 	if (t1 <> Sast.Boolean || t2 <> Sast.Boolean) then raise (Failure("Invalid use of or for operands' types")) else Sast.Boolean
  | AND -> 	if (t1 <> Sast.Boolean || t2 <> Sast.Boolean) then raise (Failure("Invalid use of and for operands' types")) else Sast.Boolean
  | NOT -> 	raise (Failure("Invalid use of ! for two operands"))
  (*| _ ->		raise (Failure("Invalid binary operator")) if this line uncomment, get a case unused warning *)

let analyze_unop (scope: symbol_table) op t1 = match op with
NOT -> 		if (t1 <> Sast.Boolean) then raise (Failure("Invalid use of ! for operand type")) else Sast.Boolean
| _ -> 		raise (Failure("Invalid unary operator"))

let convert_data_type env old_type = match old_type with
  | Ast.Void -> Sast.Void
  | Ast.Number -> Sast.Number
  | Ast.Boolean -> Sast.Boolean
  | Ast.String -> Sast.String
  | Ast.Char -> Sast.Char
  | Ast.Object(n) ->
      let obj_dec = try find_class_decl env.scope n
      with Not_found -> raise(Failure("classdecl not found")) in
      Sast.Object(obj_dec)


(* compare parameter types *)
let rec compare_p_types formalVars actualExprs = match formalVars, actualExprs with
    [], [] -> true
    |[], y::ytail  ->raise(Failure("wrong number of params"))
    | x::xtail, [] -> raise(Failure("wrong number of params")) 
    |x::[], y::y2::ytail -> raise(Failure("wrong number of params"))
    |x::x2::[], y::[] -> raise(Failure("wrong number of params"))
    |x::xtail, y::ytail -> let (_, actual_typ) = y in
        if(actual_typ) == x.vtype then begin compare_p_types xtail ytail end
        else raise(Failure("wrong parameter type"))
(* Expression Environment *)
let rec analyze_expr env = function

      (* Simple evaluation of primitives *)
      Ast.LitNum(v) -> Sast.LitNum(v), Sast.Number
    | Ast.LitBool(v) -> Sast.LitBool(v), Sast.Boolean
    | Ast.LitString(v) -> Sast.LitString(v), Sast.String
    | Ast.LitChar(v) -> Sast.LitChar(v), Sast.Char
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
          Sast.Assign(vname, (e, expr_typ)), expr_typ
    | Ast.Instantiate(objType, exprs) ->
        let objDecl = try
          find_class_decl env.scope objType
        with Not_found ->
            raise (Failure("class not found " ^ objType))
        in 
        let actual_p_typed = List.map (fun e -> analyze_expr env e) exprs in
        if (compare_p_types objDecl.cformals actual_p_typed) = true then begin
            increment_new_count(); (Sast.Instantiate(objDecl, actual_p_typed), Sast.Object(objDecl))
        end 
      else raise (Failure("invalid parameters to function"))
    | Ast.Access(objName, varName) ->
        let objDec = try find_variable env.scope objName
          with Not_found ->
          raise(Failure("object variable not found" ^ objName))
        in let classDec = try get_class_decl_from_type env.scope objDec.vtype
          with Not_found -> raise(Failure("class not found"))
        in let class_var =
        try find_class_var env.scope classDec varName
          with Not_found ->
          raise(Failure("instance variable not found" ^ varName))
        in (Sast.Access(objDec, class_var), class_var.vtype) 


    | Ast.Binop(e1, op, e2) ->
  	  let e1 = analyze_expr env e1 (* Check left and right children *)
  	  and e2 = analyze_expr env e2 in
  	  let _, t1 = e1 (* Get the type of each child *)
  	  and _, t2 = e2 in (*let valid = *)
  	  let validbinop = try analyze_binop env.scope op t1 t2
  	  with Not_found -> raise (Failure("Invalid binary operator"))
      in if validbinop = Sast.String then Sast.StrCat(e1, e2), validbinop
        else Sast.MathBinop(e1, op, e2), validbinop (* Success: result is int *)

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
      let ret_type = fdecl.freturn in

      if fname <> "say" then begin
        if (compare_p_types formal_p_list actual_p_typed) = true then
           (Sast.FCall(fdecl, actual_p_typed), ret_type)
        else raise (Failure("invalid parameters to function"))
      end
        else Sast.FCall(fdecl, actual_p_typed), ret_type

    | Ast.ACall(objName, actName, expr_list) ->
       (* Grab object variable *)
       let objDec = try find_variable env.scope objName
       with Not_found -> raise(Failure("variable not found " ^ objName))
       (* Find corresponding class variable *)
       in let classDec =  
       get_class_decl_from_type env.scope objDec.vtype

       (* Check that action is valid *)
       in let actionDec = try find_action_decl classDec.cactions actName
       with Not_found -> raise (Failure("action not found" ^ actName))
       in
          (*check that params are correct *)
            let formal_p_list = actionDec.aformals in
            let actual_p_typed = List.map( fun a -> analyze_expr env a) expr_list in
            let ret_type = actionDec.areturn in
              if (compare_p_types formal_p_list actual_p_typed) = true then
                (Sast.ACall(objDec, actionDec, actual_p_typed), ret_type)
              else raise (Failure("invalid parameters to action " ^ actName))
    | Ast.Noexpr -> Sast.Noexpr, Sast.Void
    | _ -> Sast.LitString(""), Sast.String

let type_as_string t = match t
with 
     Sast.Number -> "float"
   | Sast.Boolean -> "bool"
   | Sast.String -> "char *"
   | Sast.Char -> "char"
   | Sast.Void -> "void"
   | Sast.Object(n) -> "object" ^ n.cname

(* convert ast.var_decl to sast.variable_decl*)
(* if there's an expression, we want to check it *)
(* then add to scope's variable list *)
let check_var_decl (env: translation_environment) (var: Ast.var_decl) =
  let typ = convert_data_type env var.vtype in
    let (e, expr_typ) = analyze_expr env var.vexpr in match e
    with Sast.Noexpr -> 
            let sast_var_decl = { vtype = typ; vname = var.vname; vexpr = (e, expr_typ) }
            in env.scope.variables <- List.append env.scope.variables [sast_var_decl];
            sast_var_decl
    | _ ->  if typ <> expr_typ then begin
            raise(Failure(
              "Variable assignment does not match variable type " ^(type_as_string typ) ^ (type_as_string expr_typ)))
            end
          else begin 
            let sast_var_decl = { vtype = typ; vname = var.vname; vexpr = (e, expr_typ) }
            in env.scope.variables <- List.append env.scope.variables [sast_var_decl];
            sast_var_decl
          end

let rec analyze_stmt env = function
    Ast.Expr(e) -> Sast.Expression(analyze_expr env e) (* expression *)
  | Ast.VarDecl(var_decl) ->
          if List.exists (fun x -> x.vname = var_decl.vname) env.scope.variables then
            raise(Failure("Variable already declared in this scope"))
          else
            let sast_var = check_var_decl env var_decl in
            let _ = env.scope.variables <- sast_var :: env.scope.variables in (* save new var_decl in symbol table *)
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
      let sastexpr1 = analyze_stmt env e1 in
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

  | Ast.Block(stmts) -> let sast_blck = List.map( fun s -> analyze_stmt env s) stmts in
      Sast.Block(sast_blck)

let library_funcs = [
  {

    fname = "say";
    fformals = [{vtype = (Sast.String);
                 vname = "str";
                 vexpr = (Sast.Noexpr, Sast.String) (*will have to make void*)
                }];
    freturn = Sast.String;
    funcbody = [Sast.Expression(Sast.LitString(""), Sast.String)];
    isLib = true;
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

let analyze_func (fun_dcl : Ast.func_decl) env : Sast.function_decl = (*Why is env of type Sasy.function_decl??*)
  let name = fun_dcl.fname in
  if name = "say"
    then raise(Failure("Cannot use library function name: " ^ name))
  else begin
    let is_name_taken = is_func_name_already_used env.scope name in
    if is_name_taken != None then raise(Failure("Function name: " ^ name ^ "is already in use."))

    (*and old_formals = fun_dcl.fformals *)
    else begin
      let old_ret_type = fun_dcl.freturn
      and old_body = fun_dcl.fbody in (*?*)
      let formals = List.map(fun st-> check_var_decl env st) fun_dcl.fformals in
      let body = List.map (fun st ->  analyze_stmt env st) old_body in
      let ret_type = convert_data_type env old_ret_type in
      let _ = find_return body env ret_type in
      let sast_func_dec =    {fname = name; fformals = formals; freturn = ret_type; funcbody= body; isLib = false} in
      env.scope.functions <- List.append env.scope.functions [sast_func_dec];
      sast_func_dec
    end
  end



let analyze_classvars (var : Ast.var_decl) (class_env : translation_environment) =
  if List.exists (fun x -> x.vname = var.vname) class_env.scope.variables then
      raise(Failure("Trait already declared in this Character"))
  else
    let sast_var = check_var_decl class_env var in
    let _ = class_env.scope.variables <- sast_var :: class_env.scope.variables in (* save new class variable in symbol table *)
    sast_var

let analyze_acts (act : Ast.act_decl) (class_env : translation_environment) =
  let name = act.aname in
    if name = "say" then raise(Failure("Cannot use library function name: " ^ name))
    else 
    let ret_type = convert_data_type class_env act.areturn in 
    let formals = List.map (fun param -> analyze_classvars param class_env) act.aformals in
    let body = List.map (fun st -> analyze_stmt class_env st) act.abody in 
    {aname = name; aformals = formals; areturn = ret_type; abody = body}

let analyze_class (clss_dcl : Ast.cl_decl) (env: translation_environment) = 
  let name = clss_dcl.cname in 
  if List.exists (fun x -> x.cname = name) env.scope.characters then
    raise(Failure("Class " ^ name ^ " already exists"))
  else
    (* create new scope for the class *)
    let class_scope = {parent = None; functions = library_funcs; variables = []; characters = []} in
    let class_env = {scope = class_scope; return_type = Sast.Void} in
    let newcformals = List.map(fun f-> check_var_decl class_env f) clss_dcl.cformals in
    let inst_vars = List.map (fun st -> analyze_classvars st class_env) clss_dcl.cinstvars in
    let actions = List.map (fun a -> analyze_acts a class_env) clss_dcl.cactions in 
    let new_class = {cname = name; cinstvars = inst_vars; cactions = actions; cformals = newcformals} in
    (* add the new class to the list of classes in the symbol table *) 
    let _ = env.scope.characters <- new_class :: (env.scope.characters) in 
    new_class

let analyze_semantics prgm: Sast.program =
  let prgm_scope = {parent = None; functions = library_funcs; variables = []; characters = []} in
  let env = {scope = prgm_scope; return_type = Sast.Number} in
  let (class_decls, func_decls) = prgm  in
  let new_class_decls = List.map (fun f -> analyze_class f env) class_decls in
  let new_func_decls = List.map (fun f -> analyze_func f env) (List.rev(func_decls)) in
  (* Search for plot *)
  let _  = try
        find_plot new_func_decls
      with Not_found -> raise (Failure("No plot was found.")) in
  (new_class_decls, List.append new_func_decls library_funcs) 
