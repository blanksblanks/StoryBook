open Printf
open Ast
open Sast
open Semantic_analyzer
open Lexing

let get_op o = match o with
        Add -> "+ "
        | Sub -> "- "
        | Mult -> "* "
        | Div -> "/ "
        | Mod -> "+ "
        | Equal -> "== "
        | Neq ->  "!= "
        | Less -> "< "
        | Leq -> "<= "
        | Greater -> "> "
        | Geq -> ">= "
        | OR -> "|| "
        | AND -> "&& "
        | NOT -> "!"

let type_as_string t = match t
with Sast.Number -> "int"
   | Sast.Boolean -> "bool"
   | _ -> "int"
type preceding_code =
  Some of string
  | None

let get_bool_str b = match b
with true -> "1"
   | _ -> "0"

let rec get_expr (e, t) = match e
with Sast.LitString(s) -> (s, None)
   | Sast.LitBool(b) ->
     let b_str = get_bool_str b in
     (b_str, None)
   | Sast.LitNum(n) -> (string_of_float n, None)
   | Sast.LitChar(c) -> (Char.escaped c, None)
   | Sast.Id(var) ->
     let (expr_str, _ ) = get_expr var.vexpr in
     (expr_str, None)
   | Sast.Assign(id, e) ->
     let (exp, _) = get_expr e in
     (id ^ " = " ^ exp, None)
   | Sast.Unop(op, expr) ->
     let op_str = get_op op in
     let (expr_str, _) = get_expr expr in
     (op_str ^ "(" ^ expr_str ^ ")", None)
   | Sast.MathBinop(expr1, op, expr2) ->
     let (expr_str_1, _) = get_expr expr1 in
     let (expr_str_2, _) = get_expr expr2 in
     let op_str = get_op op in
     (expr_str_1 ^ op_str ^ expr_str_2, None)
   | Sast.StrCat(expr1, expr2) ->
     let (expr1_str, _) = get_expr expr1 in
     let (expr2_str, _) = get_expr expr2 in
     let (_, typ1) = expr1 and (_, typ2) = expr2 in
     let convert_expr1 = match typ1
     with Sast.Number -> "sprintf(buf, \"%f\", " ^ expr1_str ^ ");\n"
        | Sast.Boolean -> "sprintf(buf, " ^ expr1_str ^ " ? \"true\" : \"false\");\n"
        | Sast.String -> "sprintf(buf, " ^ expr1_str ^ ");\n"
        | Sast.Char -> "sprintf(buf, \"%c\", " ^ expr1_str ^ ");\n"
        | _ -> "" in
     let convert_expr2 = match typ2
     with Sast.Number -> "sprintf(buf + strlen(buf), \"%f\", " ^ expr2_str ^ ");\n"
        | Sast.Boolean -> "sprintf(buf + strlen(buf), " ^ expr2_str ^ " ? \"true\" : \"false\");\n"
        | Sast.String -> "sprintf(buf + strlen(buf), " ^ expr2_str ^ ");\n"
        | Sast.Char -> "sprintf(buf + strlen(buf), \"%c\", " ^ expr2_str ^ ");\n"
        | _ -> "" in
   ("a", Some("char buf[100000];\n" ^ convert_expr1 ^ convert_expr2 ^ "char *a = buf;"))
   | Sast.FCall (f_d, e_l) ->
     if f_d.fname = "say" then begin
       let (strExp, typ) = (List.nth e_l 0) in match strExp
       with Sast.LitString(s) ->
            let lit_str = (String.sub s 0 (String.length s - 1)) ^ ("\\n\"") in
            ("\tprintf" ^ " ( " ^ lit_str ^ ")", None)
          | Sast.LitNum(n) -> ("\tprintf" ^ " ( " ^ (string_of_float n) ^ ")", None)
          | Sast.Id(var) -> let (exp_det, _) = var.vexpr in
            ( match exp_det
              with Sast.LitString(s) ->
                   let lit_str = (String.sub s 0 (String.length s - 1)) ^ ("\\n\"") in
                   ("\tprintf" ^ " ( " ^ lit_str ^ ")", None)
                 | Sast.LitNum(n) -> ("\tprintf" ^ " (\"%f\"," ^ (string_of_float n) ^ ")", None)
                 | Sast.MathBinop(e1, op, e2) ->
                   let (expr_str, _) = get_expr (strExp, typ) in
                   ("\tprintf ( \"%d\\n\", " ^ expr_str ^ ")" , None)
                 | Sast.StrCat(e1, e2) ->
                   let (str_expr, prec_code) = get_expr (strExp, typ) in
                   let prec_code_str = match prec_code
                   with Some(s) -> s
                      | None -> "" in
                   let whole_str = prec_code_str ^ "\n\tprintf (\"%s\\n\"," ^str_expr ^ ")" in
                   (whole_str, None)
                 | _ -> ("", None) )
          | Sast.MathBinop(e1, op, e2) -> let (expr_str, _) = get_expr (strExp, typ) in
            ("\tprintf ( \"%d\\n\", " ^ expr_str ^ ")" , None)
          | Sast.StrCat(e1, e2) ->
            let (str_expr, prec_code) = get_expr (strExp, typ) in
            let prec_code_str = match prec_code
            with Some(s) -> s
               | None -> "" in
                 let whole_str = prec_code_str ^ "\n\tprintf (\"%s\\n\"," ^str_expr ^ ")" in
                 (whole_str, None)
   	      | _ -> ("", None)
       end
     else begin
       let param_str = List.fold_left(fun str e -> let (exp_str, _) = get_expr e in str ^ exp_str) "" e_l in
       ("\t " ^ f_d.fname ^ " " ^ " (" ^  param_str ^ " )", None)
       end;
   | _ -> ("", None)

let get_form_param (v: variable_decl) =
    let typ = type_as_string v.vtype in
    typ ^ " " ^ v.vname


let write_stmt s = match s with
| Sast.Expression(e) -> let (expr_str, _) = get_expr e in print_string expr_str; print_string ";\n"
| Sast.VarDecl(vdecl) -> let vtyp = type_as_string vdecl.vtype in
    let vname = vdecl.vname in let (vexp, _) = get_expr vdecl.vexpr in
    print_string (vtyp ^ " " ^ vname ^ " = " ^ vexp); print_string ";\n"
| Sast.Return(e) -> print_string "return "; let (expr_str, _) = get_expr e in print_string expr_str; print_string ";\n"
| _ -> let (expr_str, _) = get_expr (LitString(""), Sast.String) in print_string expr_str; print_string ";\n"


let get_formals params =
  let p_list = List.fold_left (fun str v -> let v_str = get_form_param v in str ^ v_str) "" params in
  p_list

let write_func funcdec =
	print_string ((type_as_string funcdec.freturn) ^ " ");
	if funcdec.fname = "plot"
        then print_string " main"
        else print_string (" " ^ funcdec.fname);
	let forms = get_formals funcdec.fformals in
    print_string ("(" ^ forms ^ ")");
	print_string " { \n";
	List.iter (fun s -> write_stmt s) funcdec.funcbody;
	print_string " } \n"

let print_code pgm =
	let (cdecs, funcdecs) = pgm in
    print_string "#include <stdio.h> \n #include <string.h> \n\n\t";
    let userFuncs = List.filter (fun f -> f.isLib = false) funcdecs in
      List.iter (fun f -> write_func f) userFuncs;
  flush

	let lexbuf = Lexing.from_channel stdin
	let ast = Parser.program Scanner.token lexbuf
	let sast = analyze_semantics ast
	let _ = print_code sast
