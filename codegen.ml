open Ast
open Sast
open Semantic_Analyzer
open Lexing

let sast =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in 
	analyze_semantics ast

let generate_code pgm =
let _ = print_string "#include <cstdio> Program {\n\n\t" in


let (classes, funcs) = pgm in
		(*List.iter print_class_decl (List.rev classes); *)
		List.iter print_func_decl (List.rev funcs);
		print_string "\n}\n"

in generate_code sast


