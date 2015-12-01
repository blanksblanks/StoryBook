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

let write_params params f =
	fprintf f " ( ";
    List.iter (fun v -> fprintf f " %s " v.vname) params;
	fprintf f ") "

(* Takes expression tuple and file to write to *)
let rec write_expr (e, t) f = match e with
| Sast.LitString(s) -> fprintf f "%s" s
| Sast.LitNum(n) -> fprintf f "%d" n
| Sast.Unop(op, expr) -> print_op op f; fprintf f "("; write_expr expr f; fprintf f ")"
| Sast.Binop(expr1, op, expr2) -> write_expr expr1 f; print_op op f; write_expr expr2 f
| Sast.FCall (f_d, e_l) -> 
    if f_d.fname = "say" then begin fprintf f "\tprintf"; fprintf f " ( ";
        let (strExp, typ) = (List.nth e_l 0) in match strExp with
        |Sast.LitString(s) -> let newStr = (Sast.LitString((String.sub s 0 (String.length s - 1))^("\\n\"")), Sast.String) in write_expr newStr f; fprintf f ")"
	(* trynna implement numbers and try to get them to print so that i can check binop and unop *)
	|Sast.LitNum(n) -> let newNum = (Sast.LitNum(n), Sast.Number) in write_expr newNum f; fprintf f ")"
        | _ -> fprintf f ""
    end
    else begin fprintf f  "\t %s " f_d.fname;  fprintf f " ("; List.iter (fun e -> write_expr e f) e_l; fprintf f " )"end;
| _ -> fprintf f ""


let write_stmt s f = match s with
| Sast.Expression(e) -> write_expr e f; fprintf f ";\n"
| _ -> write_expr (LitString(""), Sast.String) f; fprintf f ";\n"

let write_func funcdec f =
	fprintf f "%s " (type_as_string funcdec.freturn);
	if funcdec.fname = "plot" 
        then fprintf f " main" 
        else fprintf f " %s" funcdec.fname;
	write_params funcdec.fformals f;
	fprintf f " { \n";
	List.iter (fun s -> write_stmt s f) funcdec.funcbody;
	fprintf f " } \n"

let generate_code pgm =
	let (cdecs, funcdecs) = pgm in
    let cfile = open_out "helloWorld.c" in
    fprintf cfile "#include <stdio.h> \n\n\t";
    write_func (List.nth funcdecs 0) cfile;
    close_out cfile


	let lexbuf = Lexing.from_channel stdin
	let ast = Parser.program Scanner.token lexbuf
	let sast = analyze_semantics ast
	let _ = generate_code sast
