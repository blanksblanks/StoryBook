open Printf
open Ast
open Sast
open Cast
open Semantic_analyzer
open Lexing
open Codegen

(* current_ptr keeps track of index of each object in the array of
   malloc'ed c structs *)
let current_ptr = ref (-1)
let increment_cur_ptr() = current_ptr := !current_ptr + 1

(* current_var is an int that keeps track of the current variable name
   used in the code -- we convert this to string name *)
let current_var = ref 0
let increment_current_var() = current_var := !current_var + 1
let get_next_var_name() = increment_current_var(); "_" ^ (string_of_int !current_var)

(* Convert operations to strings *)
let get_op o = match o
with Add -> " + "
   | Sub -> " - "
   | Mult -> "* "
   | Div -> " / "
   | Mod -> " % "
   | Equal -> " == "
   | Neq ->  " != "
   | Less -> " < "
   | Leq -> " <= "
   | Greater -> " > "
   | Geq -> " >= "
   | OR -> " || "
   | AND -> " && "
   | NOT -> " !"

(* let list_type_as_string (t: Sast.list_type) = match t with
  Sast.Number -> "float"
| Sast.Boolean -> "bool"
| Sast.Char -> "char" *)

(* let dtype_to_ltype t = match t with
   Sast.Number -> (Sast.Number: Sast.list_type)
 | Sast.Boolean -> (Sast.Boolean: Sast.list_type)
 | Sast.Char -> (Sast.Char: Sast.list_type)
 | _ -> raise(Failure("Cannot have a list of this type")) *)

let type_as_string t = match t
with 
     Sast.Number -> "float"
   | Sast.Boolean -> "bool"
   | Sast.String -> "char *"
   | Sast.Char -> "char"
   | Sast.Void -> "void"
   | Sast.Object(n) -> "struct " ^ n.cname ^ " *"
   | Sast.NumberList -> "float *"
   | Sast.BooleanList -> "bool *"
   | Sast.CharList -> "char *"

let find_listAcc_type t = match t with 
    Sast.NumberList -> Sast.Number
  | Sast.BooleanList -> Sast.Boolean
  | Sast.CharList -> Sast.Char  
  | _ -> raise(Failure("Not list type"))

let get_bool_str b = match b with 
    true -> "1"
   | _ -> "0"

let get_str_len expr_str typ = match typ
with Sast.Number -> "5000"
   | Sast.Boolean -> "5"
   | Sast.String -> "strlen(" ^ expr_str ^ ")"
   | Sast.Char -> "1"
   | _ -> "10000"


let get_str_cat_code expr1_str typ1 expr2_str typ2 v_name=
     let buf_name = "buf_" ^ v_name in
     let convert_expr1 = match typ1
     with Sast.Number -> "sprintf(" ^ buf_name ^ ", \"%g\"," ^ expr1_str ^ ");\n"
        | Sast.Boolean -> "sprintf(" ^ buf_name ^ ", \"%s\", " ^ expr1_str ^ " ? \"true\" : \"false\");\n"
        | Sast.String -> "sprintf(" ^ buf_name ^ ", \"%s\"," ^ expr1_str ^ ");\n"
        | Sast.Char -> "sprintf(" ^ buf_name ^ ", \"%c\", \'" ^ expr1_str ^ "\');\n"
        | _ -> "" in
     let convert_expr2 = match typ2
     with Sast.Number -> "sprintf(" ^ buf_name ^ " + strlen(" ^ buf_name ^ "), \"%g\", " ^ expr2_str ^ ");\n"
        | Sast.Boolean -> "sprintf(" ^ buf_name ^ " + strlen(" ^ buf_name ^ "), \"%s\"," ^ expr2_str ^ " ? \"true\" : \"false\");\n"
        | Sast.String -> "sprintf(" ^ buf_name ^ " + strlen(" ^ buf_name ^ "), \"%s\"," ^ expr2_str ^ ");\n"
        | Sast.Char -> "sprintf(" ^ buf_name ^ " + strlen(" ^ buf_name ^ "), \"%c\", " ^ expr2_str ^ ");\n"
        | _ -> "" in

    let expr1_len = get_str_len expr1_str typ1 in
    let expr2_len = get_str_len expr2_str typ2 in
    let buf_code = "char " ^ buf_name ^ "[ " ^ expr1_len ^ " + " ^ expr2_len ^ " + 1];\n" in

    buf_code ^ convert_expr1 ^ convert_expr2 ^ "char *" ^ v_name ^ " = buf_" ^ v_name ^ ";"

let idx = ref (0)
let increment_idx() = idx := !idx + 1


let rec get_init_str frm actl name =
  let (actl_exp_str, prec_code) = get_expr actl in
  let init_str = prec_code ^ "\n" ^
  "((struct " ^ name ^" *)ptrs[" ^ (string_of_int !current_ptr) ^ "])  -> " ^ frm.vname ^ " = "
  ^ actl_exp_str ^ ";\n" in
  init_str

and initalize_inst_vars (forms: Sast.variable_decl list) actuals name =
  let p_list = List.fold_left (
      fun str f ->
        let actl_i = List.nth actuals !idx in
        let new_str = get_init_str f actl_i name
        in
        increment_idx();
        str ^ new_str
      ) "" forms
  in let vtable_str = "((struct " ^ name ^" *)ptrs[" ^ (string_of_int !current_ptr) ^ "])  ->" ^
      "vtable = &vtable_for_" ^ name ^ ";\n\n" in
  (p_list ^ vtable_str)


and get_expr (e, t) = match e
with Sast.LitString(s) ->  (s, "")
   | Sast.LitBool(b) -> let b_str = get_bool_str b in (b_str, "")
   | Sast.LitNum(n) -> (string_of_float n, "")
   | Sast.LitChar(c) -> ("\'" ^ Char.escaped c ^ "\'", "")
   | Sast.Id(var) -> (var.vname, "")
   | Sast.Assign(id, e) ->
     let (exp, prec_assign) = get_expr e in
     (id ^ " = " ^ exp, prec_assign)
   | Sast.Instantiate(c_dec, exprs) ->
     increment_cur_ptr();
     let rev_vars = List.rev c_dec.cinstvars in
     let _ = idx := 0 in
     let init_str = (initalize_inst_vars rev_vars exprs c_dec.cname) in
     let obj_inst_str = "\tptrs[" ^ string_of_int !current_ptr ^ "]" ^
     " = malloc((int)sizeof(struct " ^ c_dec.cname ^ " ));\n" ^ init_str in
     ("ptrs[" ^ string_of_int !current_ptr ^ "];\n", obj_inst_str)
   | Sast.ListInstantiate(typ, s) -> 
     let dtyp = type_as_string typ in
     let dataType = String.sub dtyp 0 (String.length dtyp - 1) in(* get rid of ptr *)
     let (size, prec_code) = get_expr s in
     let intSize = String.sub size 0 (String.length size - 1) in (* turn float into int *)
     ("malloc(" ^ intSize ^ " * sizeof(" ^ dataType ^ "))", prec_code)
   | Sast.ListAccess(vdecl, i) -> 
       let (indx, prec_access) = get_expr i in 
       let listId = vdecl.vname in
       (listId ^ "[(int)" ^ indx ^ "]", prec_access)
    | Sast.ListAssign(access, v) -> 
      let (elem, prec_access) = get_expr access in
      let (assn, prec_assign) = get_expr v in
      (elem ^ " = " ^ assn, prec_access ^ prec_assign) 
   | Sast.Unop(op, expr) ->
     let op_str = get_op op in let (expr_str, prec_unop) = get_expr expr in
     (op_str ^ "(" ^ expr_str ^ ")", prec_unop)
   | Sast.MathBinop(expr1, op, expr2) ->
     let (expr_str_1, prec_bin1) = get_expr expr1 in
     let (expr_str_2, prec_bin2) = get_expr expr2 in
     if op = Equal then 
          let op_str = get_op op in
		let (det1, typ1) = expr1 in
			match typ1 with 
			Sast.String -> ("strcmp(" ^ expr_str_1 ^ "," ^ expr_str_2 ^ ") " ^ op_str ^ " 0", prec_bin1^prec_bin2)
			| _ -> (expr_str_1^op_str ^ expr_str_2, prec_bin1^prec_bin2)
     else if op = Mod then
          let op_str = get_op op in
          ("(double)" ^ "((int) (" ^ expr_str_1^ ") " ^ op_str ^ "(int) ( " ^ expr_str_2 ^ "))", prec_bin1^prec_bin2)
     else
         let op_str = get_op op in
         (expr_str_1^op_str ^ expr_str_2, prec_bin1^prec_bin2)
   | Sast.StrCat(expr1, expr2) ->
     let (expr1_str, prec_strcat1) = get_expr expr1 in
     let (expr2_str, prec_strcat2) = get_expr expr2 in
     let (_, typ1) = expr1 and (_, typ2) = expr2 in
     let v_name = get_next_var_name() in
     let str_cat_code = get_str_cat_code expr1_str typ1 expr2_str typ2 v_name in
     (v_name, prec_strcat1 ^ prec_strcat2 ^ str_cat_code)
    | Sast.TraitAssign(accessVar, expr) ->
      let (varAccess, _) = get_expr accessVar in
      let (new_value, _) = get_expr expr in
      (varAccess ^ "=" ^ new_value, "")
    | Sast.Access(obj_dec, var_dec) -> (obj_dec.vname ^ " -> " ^ var_dec.vname ,"")

    | Sast.FCall (f_d, e_l) ->
      if f_d.fname = "say" then begin
        let (strExp, typ) = (List.nth e_l 0) in match strExp
        with Sast.LitString(s) ->
           let lit_str = (String.sub s 0 (String.length s - 1)) ^ ("\\n\"") in
           ("printf" ^ " ( " ^ lit_str ^ ")", "")
           | Sast.LitNum(n) -> ("printf" ^ " ( \"%g\", " ^ (string_of_float n) ^ ")", "")
           | Sast.LitBool(b) -> ("printf( \"%s\", " ^ (get_bool_str b) ^ " ? \"true\" : \"false\");\n", "")
           | Sast.LitChar(c) -> ("printf( \"%c\", \'" ^ Char.escaped c ^  "\')", "")
           | Sast.MathBinop(e1, op, e2) ->
               let (expr_str, prec_expr) = get_expr (strExp, typ) in
               if typ = Sast.Number
                 then ("printf ( \"%g\\n\", " ^ expr_str ^ ")" , prec_expr)
               else
                 ("printf ( \"%s\\n\", " ^ expr_str ^ " ? \"true\" : \"false\")", prec_expr)
           | Sast.Unop(op, e) ->
               let (expr_str, prec_expr) = get_expr(strExp, typ) in
               if typ = Sast.Number
                 then ("printf (\" oops, unops for numbers are not implemented \")", prec_expr)
               else
                 ("printf ( \"%s\\n\", " ^ expr_str ^ " ? \"true\" : \"false\")", prec_expr)
           | Sast.StrCat(e1, e2) -> let (str_expr, prec_code_str) = get_expr (strExp, typ) in
                let whole_str = prec_code_str ^ "\n\tprintf (\"%s\\n\"," ^str_expr ^ ")" in
                (whole_str, "")
           | Sast.Id(var) -> let typ = var.vtype in
              ( match typ with
                     Sast.String -> ("printf ( \"%s\\n\", " ^ var.vname ^ ")", "")
                   | Sast.Number -> ("printf (\"%g\"," ^ var.vname ^ ")", "")
                   | Sast.Boolean -> ("printf(\"%d\\n\", " ^ var.vname ^ ")", "")
                   | Sast.Char -> ("printf( \"%c\", " ^ var.vname ^  ")", "")
                   | _ -> ("", "") )
           | Sast.ListAccess(vdecl, i) -> 
               let (indx, _) = get_expr i in 
               let listId = vdecl.vname in
               let listAccess = (listId ^ "[(int)" ^ indx ^ "]") in
               let accessType = find_listAcc_type vdecl.vtype in 
               (match accessType with 
                     Sast.Number -> ("printf(\"%f\", " ^ listAccess ^ ")", "")
                   | Sast.Boolean -> ("printf(\"%d\"," ^ listAccess ^ ")", "")
                   | Sast.Char -> ("printf(\"%c\", " ^ listAccess ^ ")", "")
                   | _ -> ("", "")
               )
           | Sast.Access(objVar, instVar) ->
               let typ = instVar.vtype in
               let (expr_str, prec_code) =  get_expr (strExp, typ) in
               (match typ with
                     Sast.Number -> ("\tprintf ( \"%g\\n\", " ^ expr_str ^ ")", prec_code)
                   | Sast.Boolean ->("\tprintf (\"%d\"," ^ expr_str ^ ")", prec_code)
                   | Sast.String -> ("\tprintf(\"%s\\n\", " ^ expr_str ^ ")", prec_code)
                   | Sast.Char ->  ("\tprintf( \"%c\", " ^ expr_str ^  ")", prec_code)
                   | _ -> raise(Failure("not a printable type")))

           | Sast.FCall(f_d_inner, e_l_inner) ->
               let (inner_func_str, prec_inner_func) = get_expr (strExp, typ) in
               ( match typ with
                     Sast.String -> ("\tprintf ( \"%s\\n\", " ^ inner_func_str ^ ")", prec_inner_func)
                   | Sast.Number -> ("\tprintf (\"%g\"," ^ inner_func_str ^ ")", prec_inner_func)
                   | Sast.Boolean -> ("\tprintf(\"%d\\n\", " ^ inner_func_str ^ ")", prec_inner_func)
                   | Sast.Char -> ("\tprintf( \"%c\", " ^ inner_func_str ^  ")", prec_inner_func)
                   | _ -> ("", "") )
  	      (*  | Sast.ACall(objDec, actDec, exprs) -> *)
            | Sast.Noexpr -> ("", "")
            | _ -> ("", "")
      end
      (* Regular function call --i.e., not "say" *)
      else begin
       let (param_str, prev_code) = List.fold_left(fun str_tup e ->
           let (cur_str, cur_prec_code) = get_expr e in
           let (prev_str, prev_prec_code) = str_tup in 
           (prev_str ^ cur_str ^ ", ", prev_prec_code ^ "\n" ^ cur_prec_code)
         ) ("", "") e_l in 
        let clean_param_str =
          if (String.length param_str) > 0 then (String.sub param_str 0 ((String.length param_str) - 2))
          else param_str in 

        let fcall_str = "\t " ^ f_d.fname ^ " " ^ " (" ^  clean_param_str ^ " )" in 
        match f_d.freturn with
        | Sast.String ->
            let ret_var = get_next_var_name() in
            let call_and_store = "char *" ^ ret_var ^ " = " ^ fcall_str ^ ";\n" in 
            let save_var = get_next_var_name() in 
            let save_buf = "char " ^ save_var ^ "[strlen(" ^ ret_var ^ ")];\n" in 
            let copy = "strcpy(" ^ save_var ^ ", " ^ ret_var ^ ");\n" in 
            let free = "free(" ^ ret_var ^ ");\n" in 
            (save_var, (prev_code ^ call_and_store ^ save_buf ^ copy ^ free))

        | _ -> (fcall_str, prev_code)
     end

  (* Action call: takes in object variable declaration, action declaration,
     and actual parameters *)
  | Sast.ACall(objDec, actDec, exprs) ->
     let (param_str, prev_code) = List.fold_left(fun str_tup e ->
         let (cur_str, cur_prec_code) = get_expr e in
         let (prev_str, prev_prec_code) = str_tup in 
         (prev_str ^ cur_str ^ ", ", prev_prec_code ^ "\n" ^ cur_prec_code)
       ) ("", "") exprs in 
     let full_param_str = param_str ^ objDec.vname in 
     let access_vtbl_act = objDec.vname ^ "->vtable->" ^ actDec.aname in
     let acall_str = "\t " ^ access_vtbl_act ^ " " ^ " (" ^  full_param_str ^ " )" in 

      (* Figure out what type the return is *)
      (match actDec.areturn with
        (* If action returns a string, must free the malloc'ed string *)
        | Sast.String ->
            let ret_var = get_next_var_name() in
            let call_and_store = "char *" ^ ret_var ^ " = " ^ acall_str ^ ";\n" in 
            let save_var = get_next_var_name() in 
            let save_buf = "char " ^ save_var ^ "[strlen(" ^ ret_var ^ ")];\n" in 
            let copy = "strcpy(" ^ save_var ^ ", " ^ ret_var ^ ");\n" in 
            let free = "free(" ^ ret_var ^ ");\n" in 
            (save_var, (prev_code ^ call_and_store ^ save_buf ^ copy ^ free))
        (* If action returns anything else, no need to malloc *)
        |_ -> (acall_str, prev_code) )
 
  (* catch all *)
    | Sast.Noexpr -> ("", "")

let get_form_param (v: Sast.variable_decl) =
  let typ = type_as_string v.vtype in
  typ ^ " " ^ v.vname

let get_formals params =
  let p_list = List.fold_left (fun str v -> let v_str = get_form_param v in str ^ v_str ^ ", ") "" params in (* need to remove the last comma if function not action*)
  p_list

let rec write_stmt s = match s with 
     Sast.Expression(e) ->
      let (expr_str, prec_expr) = get_expr e in
      print_string ("\t" ^ prec_expr); print_string ";\n\t";
      print_string expr_str; print_string ";\n\t"
   | Sast.Block(stmts) -> List.iter (fun s -> write_stmt s) stmts
   | Sast.VarDecl(vdecl) -> 
      let vtyp = type_as_string vdecl.vtype in
      let vname = vdecl.vname in let (vexp, prec_expr) = get_expr vdecl.vexpr in
      print_string ( "\t" ^ prec_expr ^ vtyp ^ " " ^ vname ^ " = " ^ vexp); print_string ";\n"
   | Sast.While(e, s) -> 
      let (boolEx, prec_code) = get_expr e in 
      print_string("\t" ^ prec_code ^ "\n\t");
      print_string ("while(" ^ boolEx ^ "){ \n\t"); 
      write_stmt s;
      print_string "\n\n\t}\n\t";
   | Sast.For( ex1, ex2, ex3, s) -> 
      let (boolEx, bool_prec_code) = get_expr ex2 in 
      let (incr, incr_prec_code) = get_expr ex3 in
      print_string("\t" ^ bool_prec_code ^ "\n\t");
      print_string("\t" ^ incr_prec_code ^ "\n\t");
      write_stmt ex1;
      print_string ("\twhile(" ^ boolEx ^ "){ \n\t"); 
      write_stmt s;
      print_string (incr ^ ";\n\t");
      print_string "\n}\n\t";
   | Sast.Return(e) ->  let (expr_str, prec_code) = get_expr e in
     let (det, typ) = e in (match typ with
     | Sast.String ->
        (* If return type is a string, malloc *)
        (* MUST FREE IN FUNCTION CALLER *)
         let next_var = get_next_var_name() in 
         let malloc_str = "char *" ^ next_var ^ " = " ^
         "malloc(strlen(" ^ expr_str ^ "));\n" ^
         "strcpy(" ^ next_var ^ ", " ^ expr_str ^ ");\n" in 
         print_string(prec_code ^ "\t\n");
         print_string(malloc_str ^ "return " ^ next_var ^ ";\n")
     | _ -> print_string (prec_code ^ "\t\n");
            print_string "return "; print_string expr_str; print_string ";\n")
   | Sast.If(condExpr, ifstmt, elsestmt) ->
       let (condExprStr, condPrec) = get_expr condExpr in 
       print_string (condPrec ^ "\nif(" ^ condExprStr ^ ") {\n");
       write_stmt ifstmt;
       print_string ("\n}\nelse {");
       write_stmt elsestmt;
       print_string("}\n")
   (*| _ -> 
      let (expr_str, prec_code) = get_expr (LitString("cow"), Sast.String) in
      print_string(prec_code ^ "\t\n");
      print_string expr_str; print_string ";\n\t" *)


let write_func funcdec =
  let ret_and_name_str =
  if funcdec.fname = "plot"
    then "int main"
  else begin
    let typ_str = type_as_string funcdec.freturn in typ_str ^ " " ^ funcdec.fname
  end in
  let forms = get_formals funcdec.fformals in
  let len = String.length forms in
  let clean_forms =
    if len > 0 then (String.sub forms 0 ((String.length forms) - 2))
    else forms in (* remove the extra comma from the formals list *)
  print_string ret_and_name_str;
  print_string ("(" ^ clean_forms ^ ")");
  print_string " { \n\t";
  List.iter (fun s -> write_stmt s) funcdec.funcbody;


  (* Free global pointers at the end of main *)
  if (funcdec.fname = "plot") && (!new_count > 0) then 
  print_string "\n\tfor( int i = 0; i < (sizeof(ptrs)/sizeof(ptrs[0])); i++){\n
                      \tfree(ptrs[i]); }\n}\n "     
  else begin
    print_string " \n} \n"
  end


let rec convert_my_expr (e, t) sptr = match e with 
    Sast.Access(v, _) -> if v.istrait = true then v.vname <- sptr 
  | Sast.Assign(_, e) -> convert_my_expr e sptr
  | Sast.Unop(_, exp) -> convert_my_expr exp sptr
  | Sast.MathBinop(ex1, _, ex2) -> convert_my_expr ex1 sptr; convert_my_expr ex2 sptr
  | Sast.StrCat(ex1, ex2) -> convert_my_expr ex1 sptr; convert_my_expr ex2 sptr
  | Sast.FCall(_, el) -> List.iter(fun e -> convert_my_expr e sptr) el
  | Sast.ACall(_, _, exps) -> List.iter(fun e -> convert_my_expr e sptr) exps 
  | Sast.TraitAssign(v, _ ) -> convert_my_expr v sptr
  | _ -> ()

let rec convert_my_stmt (stmt: Sast.statement) sptr = 
  match stmt with 
     Sast.Expression(e) ->  convert_my_expr e sptr
   | Sast.Block(stmts)  -> List.iter (fun s -> convert_my_stmt s sptr) stmts
   | Sast.VarDecl(v) -> convert_my_expr v.vexpr sptr
   | Sast.While(e, s) -> convert_my_expr e sptr; convert_my_stmt s sptr
   | Sast.For(ex1, ex2, ex3, s) -> convert_my_stmt ex1 sptr; convert_my_expr ex2 sptr; 
                                   convert_my_expr ex3 sptr; convert_my_stmt s sptr
   | Sast.Return(e) -> convert_my_expr e sptr
   | Sast.If(c, ifst, elst) -> convert_my_expr c sptr; convert_my_stmt ifst sptr; convert_my_stmt elst sptr
 
let write_action s_ptr_name action =
  let ret_type = type_as_string action.areturn in 
  let ret_and_name = ret_type ^ " " ^ action.aclass ^ "_" ^ action.aname in
  let formals = get_formals action.aformals in
  let ptr_name = get_next_var_name() in 
  let ptr = ("struct " ^ s_ptr_name ^ "*" ^ ptr_name) in 
  let all_formals = (formals ^ ptr) in
  List.iter (fun s -> convert_my_stmt s ptr_name) action.abody;
  print_string ret_and_name;
  print_string ("(" ^ all_formals ^ ")");
  print_string " { \n\t";
  List.iter (fun s -> write_stmt s) action.abody;
  print_string " \n\t} \n\t"

let create_fptrs cname (cact: Sast.action_decl) = 
  let fptr = ("(*" ^ cact.aname ^ ")") in
  let freturn = type_as_string cact.areturn in
  let fforms = get_formals cact.aformals in
  let ptr_name = get_next_var_name() in 
  let ptr = ("struct " ^ cname^ " *" ^ ptr_name) in 
  let all_formals = ("(" ^ fforms ^ ptr ^ ");\n") in
  (freturn ^ fptr ^ all_formals)

let write_structs (cstruct: Cast.class_struct) =
  let dec_struct = "struct " ^ cstruct.sname ^ ";\n" in
  let vtable_def = "struct table_" ^ cstruct.sname ^ " {\n" in
  let func_ptrs = (List.fold_left(fun str f -> let f_str =  create_fptrs cstruct.sname f in str ^ f_str) "" cstruct.svtable.vfuncs) in
  let ivars = (List.map (fun v -> get_form_param v) cstruct.sivars) in
  let vtable_dec = ("static const struct table_" ^ cstruct.sname ^ " vtable_for_" ^ cstruct.sname ^ " = { \n") in
  let vtable_fncs = List.fold_left(fun str a -> str ^ cstruct.sname ^ "_" ^ a.aname ^ ", ") "\t" cstruct.svtable.vfuncs in
  let clean_vtable_fncs = (let len = String.length vtable_fncs in
    if len > 1 then (String.sub vtable_fncs 0 (len-2))
    else vtable_fncs) in
  print_string (dec_struct ^ vtable_def ^ func_ptrs ^ "\n};\n");
  print_string ("struct " ^ cstruct.sname ^ "{\n");
  print_string ("\tconst struct table_" ^ cstruct.sname ^ " *vtable;\n");
  List.iter (fun v -> print_string ("\t" ^ v ^ "; \n")) ivars;
  print_string "\n}; \n";
  List.iter (fun a -> write_action cstruct.sname a) cstruct.svtable.vfuncs;
  print_string (vtable_dec ^ clean_vtable_fncs ^ "};\n") 

let print_code pgm =
	let (cstructs, funcdecs) = pgm in
    print_string "#include <stdio.h> \n#include <string.h> \n#include <stdbool.h>\n #include <stdlib.h> \n\t";
    print_string ("void *ptrs[" ^ string_of_int !new_count ^ "];\n");
    List.iter (fun c -> write_structs c) cstructs;
    let userFuncs = List.filter (fun f -> f.isLib = false) funcdecs in
      List.iter (fun f -> write_func f) userFuncs;
  flush

  let lexbuf = Lexing.from_channel stdin
  let ast = Parser.program Scanner.token lexbuf
  let sast = analyze_semantics ast
  let cast = sast_to_cast sast 
  let _ = print_code cast



