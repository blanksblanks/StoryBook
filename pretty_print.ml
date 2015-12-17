open Printf
open Ast
open Sast
open Cast
open Semantic_analyzer
open Lexing
open Codegen

let current_var = ref 0

let increment_current_var() = current_var := !current_var + 1

let get_next_var_name() = increment_current_var(); Char.escaped(Char.chr (!current_var + 97))

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

let type_as_string t = match t
with 
     Sast.Number -> "float"
   | Sast.Boolean -> "bool"
   | Sast.String -> "char *"
   | Sast.Char -> "char"
   | _ -> "float"

let get_bool_str b = match b
with true -> "1"
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
     with Sast.Number -> "sprintf(" ^ buf_name ^ " , \"%g\", " ^ expr1_str ^ ");\n"
        | Sast.Boolean -> "sprintf(" ^ buf_name ^ ",\"%s\", " ^ expr1_str ^ " ? \"true\" : \"false\");\n"
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

let rec get_expr (e, t) = match e
with Sast.LitString(s) ->  (s, "")
   | Sast.LitBool(b) -> let b_str = get_bool_str b in (b_str, "")
   | Sast.LitNum(n) -> (string_of_float n, "")
   | Sast.LitChar(c) -> ("\'" ^ Char.escaped c ^ "\'", "")
   | Sast.Id(var) -> (var.vname, "")
   | Sast.Assign(id, e) ->
     let (exp, prec_assign) = get_expr e in
     (id ^ " = " ^ exp, prec_assign)
   | Sast.Unop(op, expr) ->
     let op_str = get_op op in let (expr_str, prec_unop) = get_expr expr in
     (op_str ^ "(" ^ expr_str ^ ")", prec_unop)
   | Sast.MathBinop(expr1, op, expr2) ->
     let (expr_str_1, prec_bin1) = get_expr expr1 in
     let (expr_str_2, prec_bin2) = get_expr expr2 in
     if op = Mod then
          let op_str = get_op op in
          (" (int) (" ^ expr_str_1^ ") " ^op_str ^ " (int)( " ^ expr_str_2 ^ ")", prec_bin1^prec_bin2)
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

    | Sast.FCall (f_d, e_l) ->
      if f_d.fname = "say" then begin
        let (strExp, typ) = (List.nth e_l 0) in match strExp
        with Sast.LitString(s) ->
             let lit_str = (String.sub s 0 (String.length s - 1)) ^ ("\\n\"") in
             ("\tprintf" ^ " ( " ^ lit_str ^ ")", "")
           | Sast.LitNum(n) -> ("\tprintf" ^ " ( \"%g\", " ^ (string_of_float n) ^ ")", "")
           | Sast.LitBool(b) -> ("\tprintf( \"%s\", " ^ (get_bool_str b) ^ " ? \"true\" : \"false\");\n", "")
           | Sast.LitChar(c) -> ("\tprintf( \"%c\", \'" ^ Char.escaped c ^  "\')", "")
           | Sast.MathBinop(e1, op, e2) ->
               let (expr_str, prec_expr) = get_expr (strExp, typ) in
               if typ = Sast.Number
                 then ("\tprintf ( \"%g\\n\", " ^ expr_str ^ ")" , prec_expr)
               else
                 ("\tprintf ( \"%s\\n\", " ^ expr_str ^ " ? \"true\" : \"false\")", prec_expr)
           | Sast.Unop(op, e) ->
               let (expr_str, prec_expr) = get_expr(strExp, typ) in
               if typ = Sast.Number
                 then ("\tprintf (\" oops, unops for numbers are not implemented \")", prec_expr)
               else
                 ("\tprintf ( \"%s\\n\", " ^ expr_str ^ " ? \"true\" : \"false\")", prec_expr)
           | Sast.StrCat(e1, e2) -> let (str_expr, prec_code_str) = get_expr (strExp, typ) in
                let whole_str = prec_code_str ^ "\n\tprintf (\"%s\\n\"," ^str_expr ^ ")" in
                (whole_str, "")
           | Sast.Id(var) -> let typ = var.vtype in
              ( match typ
                with Sast.String -> ("\tprintf ( \"%s\\n\", " ^ var.vname ^ ")", "")
                   | Sast.Number -> ("\tprintf (\"%g\"," ^ var.vname ^ ")", "")
                   | Sast.Boolean -> ("\tprintf(\"%d\\n\", " ^ var.vname ^ ")", "")
                   | Sast.Char -> ("\tprintf( \"%c\", " ^ var.vname ^  ")", "")
                   | _ -> ("", "") )

            | Sast.FCall(f_d_inner, e_l_inner) ->
                 let (inner_func_str, prec_inner_func) = get_expr (strExp, typ) in
                 ( match typ
                  with Sast.String -> print_string inner_func_str; ("\tprintf ( \"%s\\n\", " ^ inner_func_str ^ ")", prec_inner_func)
                     | Sast.Number -> print_string inner_func_str; ("\tprintf (\"%g\"," ^ inner_func_str ^ ")", prec_inner_func)
                     | Sast.Boolean -> ("\tprintf(\"%d\\n\", " ^ inner_func_str ^ ")", prec_inner_func)
                     | Sast.Char -> ("\tprintf( \"%c\", " ^ inner_func_str ^  ")", prec_inner_func)
                     | _ -> ("", "") )
  	        | _ -> ("char * = \"cow\"", "")
      end
      else begin
        let param_str = List.fold_left(fun str e -> let (exp_str, _) = get_expr e in
        str ^exp_str) "" e_l in
        ("\t " ^ f_d.fname ^ " " ^ " (" ^  param_str ^ " )", "") end;

  (* catch all *)
  | _ -> ("char * = \"meow\"", "")

let get_form_param (v: Sast.variable_decl) =
  let typ = type_as_string v.vtype in
  typ ^ " " ^ v.vname


let rec write_stmt s = match s with 
     Sast.Expression(e) ->
      let (expr_str, prec_expr) = get_expr e in
      print_string prec_expr; print_string ";\n";
      print_string expr_str; print_string ";\n"
   | Sast.Block(stmts) -> List.iter (fun s -> write_stmt s) stmts
   | Sast.VarDecl(vdecl) -> 
      let vtyp = type_as_string vdecl.vtype in
      let vname = vdecl.vname in let (vexp, _) = get_expr vdecl.vexpr in
      print_string (vtyp ^ " " ^ vname ^ " = " ^ vexp); print_string ";\n"
   | Sast.While(e, s) -> 
      let (boolEx, _) = get_expr e in 
      print_string ("while(" ^ boolEx ^ "){ \n"); 
      write_stmt s;
      print_string "}\n";
   | Sast.For( ex1, ex2, ex3, s) -> 
      let (boolEx, _) = get_expr ex2 in 
      let (incr, _) = get_expr ex3 in
      write_stmt ex1;
      print_string ("while(" ^ boolEx ^ "){ \n"); 
      write_stmt s;
      print_string (incr ^ ";\n");
      print_string "}\n";
   | Sast.Return(e) ->  let (expr_str, prec_code_str) = get_expr e in
      print_string prec_code_str; print_string "return "; print_string expr_str; print_string ";\n"
   | _ -> 
      let (expr_str, _) = get_expr (LitString("cow"), Sast.String) in
      print_string expr_str; print_string ";\n"


let get_formals params =
  let p_list = List.fold_left (fun str v -> let v_str = get_form_param v in str ^ v_str ^ ",") "" params in (* need to remove the last comma if function not action*)
  p_list

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
    if len > 0 then begin print_string (string_of_int len); print_string forms; (String.sub forms 0 ((String.length forms) - 1)) end
    else forms in (* remove the extra comma from the formals list *)
  print_string ret_and_name_str;
  print_string ("(" ^ clean_forms ^ ")");
  print_string " { \n";
  List.iter (fun s -> write_stmt s) funcdec.funcbody;
  print_string " } \n"

let write_action s_ptr action =
  let ret_type = type_as_string action.areturn in 
  let ret_and_name = ret_type ^ " " ^ action.aname in
  let formals = get_formals action.aformals in
  let ptr_name = (String.get s_ptr 0) in (* use the first letter of the character as the ptr id *)
  let ptr = ("struct " ^ s_ptr ^ "*" ^ Char.escaped ptr_name) in 
  let all_formals = (formals ^ ptr) in
  print_string ret_and_name;
  print_string ("(" ^ all_formals ^ ")");
  print_string " { \n";
  List.iter (fun s -> write_stmt s) action.abody;
  print_string " } \n"

let write_structs (cstruct: Cast.class_struct) =
  let ivars = List.map (fun v -> get_form_param v) cstruct.sivars in
  print_string ("struct " ^ cstruct.sname ^ "{");
  List.iter (fun v -> print_string (v ^ "; \n")) ivars;
  print_string "}; \n"

let print_code pgm =
	let (cstructs, cvtables, funcdecs) = pgm in
    print_string "#include <stdio.h> \n#include <string.h> \n#include <stdbool.h>\n\t";
    List.iter (fun c -> write_structs c) cstructs;
    let userFuncs = List.filter (fun f -> f.isLib = false) funcdecs in
      List.iter (fun f -> write_func f) userFuncs;
      print_string "****************\n";
      print_string(string_of_int (List.length cstructs));
      List.iter (fun vtable -> List.iter (fun a -> write_action vtable.class_name a) vtable.vfuncs) cvtables;
  flush

  let lexbuf = Lexing.from_channel stdin
  let ast = Parser.program Scanner.token lexbuf
  let sast = analyze_semantics ast
  let cast = sast_to_cast sast 
  let _ = print_code cast



