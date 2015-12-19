open Ast
open Sast

type class_struct = {
	sname: string;
	sivars: Sast.variable_decl list;
	(* sparent: Sast.class_decl; *)
	svtable: vtable
}

and vtable = {
	class_name: string; (* will tell us the name of the struct to create a ptr to *)
	vfuncs: action_decl list;
}

and prgrm = class_struct list * Sast.function_decl list