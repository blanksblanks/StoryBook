open Ast
open Sast
open Semantic_Analyzer
open Lexing

let sast =
	let lexbuf = Lexing.from_channel stdin get_instance_name in
	let ast = Parser.program Scanner.token lexbuf in 
	check_program ast

(*let print_op = function
	Add -> print_string "+ "
	| Sub -> print_string "- "
	| Mult -> print_string "* "
	| Div -> print_string "/ "
	| Mod -> print_string "% "
	| Equal -> print_string "== "
	| Neq -> print_string "!= "
	| Less -> print_string "< " 
	| Leq -> print_string "<= "
	| Greater -> print_string "> "
	| Geq -> print_string ">= "
	| OR -> print_string "|| "
	| AND -> print_string "&& "
	| NOT -> print_string "!" *)


let rec print_expr (expr : Sast.expression) =
	let (expr_det, _) = expr_det in match  expr_det with
    (*Lit_int(val) -> print_string (string_of_int val) *)
    | Lit_bool(val) -> print_string (string_of_bool val)
    | Lit_string(val) -> print_string (val)
    | Lit_Char(val) -> print_string (string_of_char val)
    | Noexpr -> print_string ""
    (* ids are stored as U-Vars, not I-Vars--just type and label *)
    | Id(var) -> let (type_label, name) = var in print_string (name) ^ " .")
	(* this is a hack. fix later *)
	| FCall(f, param_list) ->
		if f.fname = "say" then print_string "\n\t std::printf(%s," ^ (Array.get param_list 0) ^ ");"

let rec print_stmt = function
	Block(stmt_list) -> print_string "{";  List.iter print_stmt (List.rev stmt_list); print_string "}\n"
	| Expr(expr) -> print_expr expr; print_string ";"
	| Return(expr) -> print_string "return "; print_expr expr; print_string ";"
	| If(expr, ifstmt, elsestmt) -> print_string "if ("; print_expr expr; print_string ") ";
		print_stmt ifstmt; print_string "else "; print_stmt elsestmt;
	| For(varInitExp, stopCondExpr, iterExpr, loopstmt) -> print_string "for ("; print_expr varInitExp print_string ";";
		print_expr stopCondExpr; print_string ";";
	    print_expr expr3; print_string ")"; print_stmt loopstmt;  
	| While(conditionExpr, loopstmt) -> print_string "while ("; print_expr conditionExpr; print_string ")";
		print_stmt loopstmt

let print_func_decl (f : Sast.function_decl) =
	if f.fname = "plot" then 
		(print_string "int main() {\n";
		List.iter print_stmt (List.rev f.fbody);
		print_string "\n}")
	else
		(
			print_string " static ";
			print_var_types f.ftype;
			print_string f.fname; 
			print_string "(";
			print_param_list (List.rev f.checked_formals); 
			print_string ") {\n";
			List.iter print_var_decl (List.rev f.checked_locals); 
			List.iter print_stmt (List.rev f.checked_body);
			List.iter print_unit_decl (List.rev f.checked_units); 
			print_string "}\n"
		)

let generate_code pgm =
let _ = print_string "#include <cstdio> Program {\n\n\t" in


let (classes, funcs) = pgm in
		(*List.iter print_class_decl (List.rev classes); *)
		List.iter print_func_decl (List.rev funcs);
		print_string "\n}\n"

in generate_code sast


