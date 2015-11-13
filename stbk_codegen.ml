open Ast
open Sast

let print_op = function
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
	| NOT -> print_string "!" 

let get_instance_name = function
	U_Var(_, str) -> str
	(* if not a Variable we drop the unnecessary stuff *)
	| U_Var(_, str) -> str (*trait vs variable decision*)
	| U_Var(Object(_) _) -> str (*need to add objects to a list as they come to keep track of them and use them later on*)
	| I_Var(_, str, _) -> str
	| I_Var(Object(_), str, _) -> str


