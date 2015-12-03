open Printf
open Ast
open Sast
open Semantic_analyzer
open Lexing

let print_op o f = match o with 
        Add -> fprintf f "+ "
        | Sub -> fprintf f "- "
        | Mult -> fprintf f "* "
        | Div -> fprintf f "/ "
        | Mod -> fprintf f "+ "
        | Equal -> fprintf f "== "
        | Neq -> fprintf f "!= "
        | Less -> fprintf f "< " 
        | Leq -> fprintf f "<= "
        | Greater -> fprintf f "> "
        | Geq -> fprintf f ">= "
        | OR -> fprintf f "|| "
        | AND -> fprintf f "&& "
        | NOT -> fprintf f "!" 

let type_as_string t = match t with
  |Sast.Number -> "int"
  |Sast.Boolean -> "bool"
  |_ -> "int"

let write_params params =
	print_string " ( ";
    List.iter (fun v -> print_string (" "^v.vname^" ")) params;
	print_string ") "

(* Takes expression tuple and file to write to *)

let rec write_expr (e, t) f = match e with
| Sast.LitString(s) -> fprintf f "%s" s
| Sast.LitNum(n) -> fprintf f "%f" n
| Sast.Unop(op, expr) -> print_op op f; fprintf f "("; write_expr expr f; fprintf f ")"
| Sast.Binop(expr1, op, expr2) -> write_expr expr1 f; print_op op f; write_expr expr2 f

let rec write_expr (e, t) = match e with
| Sast.LitString(s) -> print_string s
| Sast.LitNum(n) -> print_float n

| Sast.FCall (f_d, e_l) -> 
    if f_d.fname = "say" then begin print_string "\tprintf"; print_string " ( ";
        let (strExp, typ) = (List.nth e_l 0) in match strExp with

        |Sast.LitString(s) -> let newStr = (Sast.LitString((String.sub s 0 (String.length s - 1))^("\\n\"")), Sast.String) in write_expr newStr; print_string ")"
	| _ -> print_string "" 
	(* trynna implement numbers and try to get them to print so that i can check binop and unop *)
	|Sast.LitNum(n) -> let newNum = (Sast.LitNum(n), Sast.Number) in write_expr newNum ; print_string ")"
        | _ -> print_string ""

       

    end
    else begin print_string  ("\t "^f_d.fname^" ");  print_string " ("; List.iter (fun e -> write_expr e) e_l; print_string " )"end;
| _ -> print_string ""


let write_stmt s = match s with
| Sast.Expression(e) -> write_expr e; print_string ";\n"
| Sast.Return(e) -> print_string "return "; write_expr e; print_string ";\n"
| _ -> write_expr (LitString(""), Sast.String); print_string ";\n"

let write_func funcdec =
	print_string ((type_as_string funcdec.freturn)^" ");
	if funcdec.fname = "plot" 
        then print_string " main" 
        else print_string (" "^funcdec.fname);
	write_params funcdec.fformals;
	print_string " { \n";
	List.iter (fun s -> write_stmt s) funcdec.funcbody;
	print_string " } \n"

let generate_code pgm =
	let (cdecs, funcdecs) = pgm in
    print_string "#include <stdio.h> \n\n\t";
    write_func (List.nth funcdecs 0);
    flush

	let lexbuf = Lexing.from_channel stdin
	let ast = Parser.program Scanner.token lexbuf
	let sast = analyze_semantics ast
	let _ = generate_code sast
