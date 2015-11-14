open Ast
open Sast
open Semantic_Analyzer
open Lexing

    let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in 
	analyze_semantics ast



let type_as_string t = match t with
  |Sast.Number -> "int"
  |Sast.Boolean -> "bool"
  |_ -> "int"

let write_params params f = 
	fprintf f " ( ";
    List.iter (fun v -> fprintf " %s " v.vname) f.fformals
	fprintf f ") ";

(* Takes expression tuple and file to write to *)
let write_expr (e, t) f = match e with
| Sast.LitString(s) -> fprintf f "%s" s
| _ -> fprintf f "";

let write_stmt stmt s f = match stmt with
| Sast.Expression(e) -> write_expr e f
| _ -> write_expr (LitString(""), Sast.String) f;

let write_func funcdec f =
	fprintf f "%s " type_as_string f.fname;
	if f.fname = "plot" then fprintf f " main" else fprintf f " %s" f.name;
	write_params f.formals f;
	fprintf f " { \n";
	List.iter (fun s -> write_stmt s f) f.body;
	fprintf f " } \n";

let generate_code pgm =
	let (cdecs, funcdecs) = pgm in 
    let cfile = open_out "HelloWorld.c" in
    fprintf cfile "#include <cstdio> Program {\n\n\t";
    fprintf cfile "int main ()";
    List.iter (fun f -> write_func f cfile) funcsdecs;
    close_out cfile;

let _ = generate_code sast;


