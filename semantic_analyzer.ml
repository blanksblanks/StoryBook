open Ast
open Sast

(* keep track of amount of objects newed for memory management--freeing*)
let new_count = ref 0 
let increment_new_count() = new_count := !new_count + 1

type symbol_table = {
  name : string;
  parent : symbol_table option;
  mutable functions: Sast.function_decl list;
  mutable variables : Sast.variable_decl list;
  mutable characters: Sast.class_decl list;
  mutable actions: Sast.action_decl list; 
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
    | _ -> raise (Failure("variable not found " ^ name))

(* Find Function *)
let rec find_class_decl (scope: symbol_table) name =
  try
     List.find(fun c -> c.cname = name) scope.characters
  with Not_found ->
  match scope.parent with
    Some(parent) -> find_class_decl parent name
  | _ -> raise (Failure("Character '" ^ name ^ "' not found"))


let rec find_class_var (scope: symbol_table) c_dec name =
  try List.find(fun v-> v.vname = name) c_dec.cinstvars
  with Not_found ->
      raise(Failure("invalid trait name: " ^ name))

let find_character (scope: symbol_table) = 
   let len = List.length scope.characters in
   match len with 
     0 -> 
      ( match scope.parent with
         Some(parent) -> List.nth parent.characters 0
       | _ -> raise(Failure("No inherited characters"))
      )
   | _ -> List.nth scope.characters 0

let get_class_decl_from_type (scope: symbol_table) ctype =
  match ctype with
  Sast.Object(typDecl) -> find_class_decl scope typDecl.cname
  | _ -> raise(Failure("not an object. can't access instance vars"))

let find_action_decl (actions : Sast.action_decl list) name className =
  try
    List.find(fun a -> (a.aname = name)) actions
  with Not_found -> 
    try
      List.find(fun a -> (a.aname = (className ^ "_" ^ name))) actions
    with Not_found -> raise (Failure("action not found " ^ name))

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

let rec type_as_string t = match t
with
     Sast.Number -> "float"
   | Sast.Boolean -> "bool"
   | Sast.String -> "char *"
   | Sast.Char -> "char"
   | Sast.Void -> "void"
   | Sast.Object(n) -> "object" ^ n.cname
   | Sast.NumberList -> "float list"
   | Sast.BooleanList -> "bool list"
   | Sast.CharList -> "char list"

let find_listAcc_type t = match t with 
    Sast.NumberList -> Sast.Number
  | Sast.BooleanList -> Sast.Boolean
  | Sast.CharList -> Sast.Char  
  | _ -> raise(Failure("Not list type"))

let rec convert_data_type env old_type = match old_type with
  | Ast.Void -> Sast.Void
  | Ast.Number -> Sast.Number
  | Ast.Boolean -> Sast.Boolean
  | Ast.String -> Sast.String
  | Ast.Char -> Sast.Char
  | Ast.Object(n) ->
      let obj_dec = try find_class_decl env.scope n
      with Not_found -> raise(Failure("classdecl not found")) in
      Sast.Object(obj_dec)
  | Ast.NumberList -> Sast.NumberList
  | Ast.BooleanList -> Sast.BooleanList
  | Ast.CharList -> Sast.CharList

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
    | Ast.TraitAssign(objAccess, ex) -> 
        let (var, vtype ) = analyze_expr env objAccess in 
        let (e, exp_type) = analyze_expr env ex in
        if vtype <> exp_type then
          raise(Failure("Incorrect type assignment to character trait"))
        else
          Sast.TraitAssign((var, vtype), (e, exp_type)), exp_type 
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
    | Ast.ListInstantiate(list_type, s) -> 
      (
        let ltype = convert_data_type env list_type in 
        let (size, typ) = analyze_expr env s in
        if typ = Sast.Number then 
          (Sast.ListInstantiate(ltype, (size, typ)), ltype)
        else raise(Failure("Must specify size of list as number"))
      ) 
    | Ast.ListAccess(id, indx) -> (* right now we don't prevent access of unassigned list elements, should be ok. *)
      (
        let var = try
          find_variable env.scope id
        with Not_found ->
          raise (Failure("Undeclared identifier " ^ id)) in 
        let (e, etype) = analyze_expr env indx in
        let accessType = find_listAcc_type var.vtype in
        (* do we need to check size of list here? idk how, maybe it's hard to keep track of that info...*)
        (* maybe we need an ocaml record for this? ugh, idk... *)
        if etype <> Sast.Number then
          raise(Failure("Must access list element with number expression"))
        else 
          (Sast.ListAccess(var, (e, etype)), accessType)
      )
    | Ast.ListAssign(access, assn) -> 
      (
        let (access, etype) = (analyze_expr env access) in
        let (new_val, vtype) = analyze_expr env assn in

        if etype <> vtype then 
          raise(Failure("Assignment value type does not match type of list element"))
        else 
        (Sast.ListAssign((access, etype), (new_val, vtype)), vtype)
      )
    | Ast.Access(objName, varName) -> (* character access *)
      (* "self" reference *)
      (
        if (objName = "my") then begin
          let classDec = find_character env.scope in (*only class dec in scope is itself, may be in outer scope because of block *)
          let classVar = try find_class_var env.scope classDec varName
            with Not_found ->
            raise(Failure("instance variable not found" ^ varName))
          in let objVar = {vtype = Object(classDec); vname = ""; vexpr = (Sast.Noexpr, Sast.Void); istrait = true } in
          (Sast.Access(objVar, classVar), classVar.vtype) 
        end
        (* Regular access *)
        else begin
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
        end 
      )
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
       in let actionDec = try find_action_decl classDec.cactions actName classDec.cname
       with Not_found -> raise (Failure("action not found " ^ actName))
       in
          (*check that params are correct *)
            let formal_p_list = actionDec.aformals in
            let actual_p_typed = List.map( fun a -> analyze_expr env a) expr_list in
            let ret_type = actionDec.areturn in
              if (compare_p_types formal_p_list actual_p_typed) = true then
                (Sast.ACall(objDec, actionDec, actual_p_typed), ret_type)
              else raise (Failure("invalid parameters to action " ^ actName))
    | Ast.Noexpr -> Sast.Noexpr, Sast.Void

(* convert ast.var_decl to sast.variable_decl*)
(* if there's an expression, we want to check it *)
(* then add to scope's variable list *)
let check_var_decl (env: translation_environment) (var: Ast.var_decl) =
  let reserve_var_names = Str.regexp "['_']['0'-'9']*" in
  let is_reserved = Str.string_match reserve_var_names var.vname 0 in
  if is_reserved <> true then begin
    let typ = convert_data_type env var.vtype in
    let (e, expr_typ) = analyze_expr env var.vexpr in match e
      (* If Uninitialized and var type is a character, throw error *)
      (* Else, the variable is valid *)
      with Sast.Noexpr -> 
              (match typ with
              Sast.Object(o) -> raise(Failure("must assign to character variable on declaration"))
              | _ -> 
                  let sast_var_decl = { vtype = typ; vname = var.vname; vexpr = (e, expr_typ); istrait = false }
                  in env.scope.variables <- List.append env.scope.variables [sast_var_decl];
                  sast_var_decl)
      (* If variable is initialized, check that the two types match *)
      | _ ->  if typ <> expr_typ then begin
              raise(Failure(
                "Variable assignment does not match variable type " ^(type_as_string typ) ^ " " ^ (type_as_string expr_typ)))
              end
            else begin 
              let sast_var_decl = { vtype = typ; vname = var.vname; vexpr = (e, expr_typ); istrait = false }
              in env.scope.variables <- List.append env.scope.variables [sast_var_decl];
              sast_var_decl
            end
  end
  else begin
    raise(Failure(
      "variable name " ^ var.vname ^ "invalid. cannot use \"_\"" ^ "or \"_\" followed only by numerical digits"
    )) end

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
  | Ast.Block(stmts) -> 
      let scope' = {name = "new block"; parent = Some(env.scope); functions = []; variables = []; characters = []; actions = []}
      in let env' = { env with scope = scope'} in
      let sast_blck =
          List.map( fun s -> analyze_stmt env' s) stmts in
          Sast.Block(sast_blck)
      let library_funcs = [
        {

          fname = "say";
          fformals = [{vtype = (Sast.String);
                       vname = "str";
                       vexpr = (Sast.Noexpr, Sast.String); (*will have to make void*)
                       istrait = false
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
      let ret_type = convert_data_type env old_ret_type in
      let formals = List.map(fun st-> check_var_decl env st) fun_dcl.fformals in
      env.scope.functions <- List.append env.scope.functions [{fname=name; fformals = formals; freturn = ret_type; funcbody= []; isLib = false}];
      let body = List.map (fun st ->  analyze_stmt env st) old_body in
      let _ = find_return body env ret_type in
      let sast_func_dec =    {fname = name; fformals = formals; freturn = ret_type; funcbody= body; isLib = false} in
      env.scope.functions <- List.filter (fun f -> f.fname <> name) env.scope.functions; (* remove dummy func for recursion *)
      env.scope.functions <- List.append env.scope.functions [sast_func_dec]; (* add real func *)
      sast_func_dec
    end
  end

let has_parent (scope : symbol_table) = 
  match scope.parent with
    Some(parent) -> (true, parent)
  | _ -> (false, scope)

let check_parent (var : Ast.var_decl) (class_env : translation_environment) = 
    let (exists, parent) = has_parent class_env.scope in 
    if exists then 
       if List.exists (fun x -> x.vname = var.vname) parent.variables then
          raise(Failure("Cannot override inherited trait: " ^ var.vname))
        else if List.exists (fun x -> x.vname = var.vname) class_env.scope.variables then
          raise(Failure("Trait " ^ var.vname ^ " already declared in this Character"))

(* Check trait not declared twice. Don't allow overriding of inherited traits. *)
let analyze_classvars (var : Ast.var_decl) (class_env : translation_environment) =
        let _ = check_parent var class_env in 
        let sast_var = check_var_decl class_env var in
        let _ = class_env.scope.variables <- sast_var :: class_env.scope.variables in (* save new class variable in symbol table *)
        sast_var

(*     match class_env.scope.parent with
      Some(parent) -> 
        if List.exists (fun x -> x.vname = var.vname) parent.variables then
          raise(Failure("Cannot override inherited trait: " ^ var.vname))
     | _ -> 
        if List.exists (fun x -> x.vname = var.vname) class_env.scope.variables then
          raise(Failure("Trait " ^ var.vname ^ " already declared in this Character")) *)
      
let analyze_acts (act : Ast.act_decl) (class_env : translation_environment) =
  if List.exists (fun x -> x.aname = act.aname) class_env.scope.actions then
    raise(Failure("Action " ^ act.aname ^ " already declared for this character"))
  else
  let name = act.aname in
    if name = "say" then raise(Failure("Cannot use library function name: " ^ name))
    else 
    let ret_type = convert_data_type class_env act.areturn in 
    let formals = List.map (fun param -> check_var_decl class_env param) act.aformals in
    let body = List.map (fun st -> analyze_stmt class_env st) act.abody in 
    let cdecl = find_character class_env.scope in
    let sast_act = {aname = name; aclass = cdecl.cname; aformals = formals; areturn = ret_type; abody = body} in
    let _ = class_env.scope.actions <- sast_act :: class_env.scope.actions in 
    sast_act

let find_parent parent child (env: translation_environment)= 
  (*if parent and child name same, then no inheritance, otherwise yes inheritance*)
  if parent <> child then
    (* If inheriting, find parent class *) 
    List.find (fun c -> c.cname = parent) env.scope.characters
  else {cname = child; cparent = child; cinstvars = []; cactions = []; cformals = []} 

let analyze_class (clss_dcl : Ast.cl_decl) (env: translation_environment) = 
  let name = clss_dcl.cname in 
  let parent = clss_dcl.cparent in
  if List.exists (fun x -> x.cname = name) env.scope.characters then
    raise(Failure("Character " ^ name ^ " already exists"))
  else if (parent <> name) && ((List.exists (fun x -> x.cname = clss_dcl.cparent) env.scope.characters) = false)
      then raise(Failure("Character " ^ clss_dcl.cparent ^ " does not exist"))
  else
    let full_parent =  find_parent parent name env in
    (* First get parent instance variables and actions and store *)
    let parent_acts = 
      if full_parent.cname <> name then 
        List.map(fun a -> 
          {aname = (name ^ "_" ^ a.aname); aclass = name; aformals = a.aformals; areturn = a.areturn; abody = a.abody}
        ) full_parent.cactions 
      else [] in
    let parent_ivars = if full_parent.cname <> name then full_parent.cinstvars else [] in

   (* create new scope for the class *)
   (* let self = {cname = name; cinstvars = []; cactions = []; cformals = []} in *)
    let class_scope =
       {name = name; parent = None; functions = library_funcs; variables = []; characters = []; actions = []} in
    let class_env = {scope = class_scope; return_type = Sast.Void} in
    
    (* Now check current inst vars and formals *)
    let newcformals = List.map(fun f-> check_var_decl class_env f) clss_dcl.cformals in
    let inst_vars = List.map (fun st -> analyze_classvars st class_env) clss_dcl.cinstvars in

    (* Combine parent and child instance variables and formal parameters *)
    let all_ivars = inst_vars @ parent_ivars in
    let all_formals = full_parent.cformals @ newcformals in

    (* Add class to it's own character scope list so that "self" references work *)
    class_env.scope.characters <-
        {cname = name; cparent = name; cinstvars = all_ivars; cactions = parent_acts; cformals = all_formals} :: class_env.scope.characters;
    let all_actions = (List.map (fun a -> analyze_acts a class_env) clss_dcl.cactions) @ parent_acts in
    let new_class = {cname = name; cparent = full_parent.cname; cinstvars = all_ivars; cactions = all_actions; cformals = all_formals} in
    (* add the new class to the list of classes in the symbol table *) 
    let _ = env.scope.characters <- new_class :: (env.scope.characters) in 
    new_class

let analyze_semantics prgm: Sast.program =
  let prgm_scope = {name= "prgrm"; parent = None; functions = library_funcs; variables = []; characters = []; actions = []} in
  let env = {scope = prgm_scope; return_type = Sast.Number} in
  let (class_decls, func_decls) = prgm  in
  let new_class_decls = List.map (fun f -> analyze_class f env) (List.rev(class_decls)) in
  let new_func_decls = List.map (fun f -> analyze_func f env) (List.rev(func_decls)) in
  (* Search for plot *)
  let plot_decl= try
        find_plot new_func_decls
      with Not_found -> raise (Failure("No plot was found.")) in
  match plot_decl.freturn with
  Sast.Void -> (new_class_decls, List.append new_func_decls library_funcs) 
  | _ -> raise(Failure("plot cannot return anything"))
