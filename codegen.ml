open Printf
open Ast
open Sast
open Semantic_analyzer
open Lexing

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
| Sast.LitString(s) -> fprintf f "\t%s" s
| Sast.FCall (f_d, e_l) -> fprintf f " \t%s " f_d.fname;
						   fprintf f " (";
						   List.iter (fun e -> write_expr e f) e_l;
						   fprintf f " )";
| _ -> fprintf f ""

let write_stmt s f = match s with
| Sast.Expression(e) -> write_expr e f; fprintf f ".\n"
| _ -> write_expr (LitString(""), Sast.String) f; fprintf f ".\n"

let write_func funcdec f =
	fprintf f "%s " (type_as_string funcdec.freturn);
	if funcdec.fname = "plot" then fprintf f " main" else fprintf f " %s" funcdec.fname;
	write_params funcdec.fformals f;
	fprintf f " { \n";
	List.iter (fun s -> write_stmt s f) funcdec.funcbody;
	fprintf f " } \n"

let generate_code pgm =
	let (cdecs, funcdecs) = pgm in 
    let cfile = open_out "HelloWorld.c" in
    fprintf cfile "#include <cstdio> Program {\n\n\t";
    write_func (List.nth funcdecs 0) cfile;
    close_out cfile


	let lexbuf = Lexing.from_channel stdin
	let ast = Parser.program Scanner.token lexbuf
	let sast = analyze_semantics ast
	let _ = generate_code sast


