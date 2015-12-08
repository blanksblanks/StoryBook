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

let type_as_string t = match t with
  |Sast.Number -> "int"
  |Sast.Boolean -> "bool"
  |_ -> "int"

let write_params params =
	print_string " ( ";
    List.iter (fun v -> print_string (" "^v.vname^" ")) params;
	print_string ") "

let rec get_expr (e, t) = match e with
| Sast.LitString(s) ->  s
| Sast.LitNum(n) -> string_of_float n
| Sast.Unop(op, expr) -> let op_str = get_op op in let expr_str = get_expr expr in
    op_str^ "("^ expr_str^")"
| Sast.MathBinop(expr1, op, expr2) -> let expr_str_1 = get_expr expr1 in let expr_str_2 = get_expr expr2 in
    let op_str = get_op op in expr_str_1^op_str^expr_str_2
| Sast.FCall (f_d, e_l) -> 
    if f_d.fname = "say" then begin
        let (strExp, typ) = (List.nth e_l 0) in match strExp with

        |Sast.LitString(s) -> let lit_str = (String.sub s 0 (String.length s - 1))^("\\n\"") in
           "\tprintf" ^ " ( " ^ lit_str^ ")"
        |Sast.LitNum(n) -> "\tprintf" ^ " ( " ^ (string_of_float n)^ ")"
	| _ -> "" 
	(* trynna implement numbers and try to get them to print so that i can check binop and unop *) 

    end
    else begin let param_str = List.fold_left(fun str e -> let exp_str = get_expr e in str^exp_str) "" e_l in
     "\t " ^ f_d.fname ^ " " ^ " (" ^  param_str ^ " )"end;
| _ -> ""


let write_stmt s = match s with
| Sast.Expression(e) -> print_string (get_expr e); print_string ";\n"
| Sast.Return(e) -> print_string "return "; print_string (get_expr e); print_string ";\n"
| _ -> print_string (get_expr (LitString(""), Sast.String)); print_string ";\n"

let write_func funcdec =
	print_string ((type_as_string funcdec.freturn)^" ");
	if funcdec.fname = "plot" 
        then print_string " main" 
        else print_string (" "^funcdec.fname);
	write_params funcdec.fformals;
	print_string " { \n";
	List.iter (fun s -> write_stmt s) funcdec.funcbody;
	print_string " } \n"

let print_code pgm =
	let (cdecs, funcdecs) = pgm in
    print_string "#include <stdio.h> \n\n\t";
    write_func (List.nth funcdecs 0);
    flush

	let lexbuf = Lexing.from_channel stdin
	let ast = Parser.program Scanner.token lexbuf
	let sast = analyze_semantics ast
	let _ = print_code sast
