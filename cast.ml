open Ast
open Sast

(* Objects in storybook are converted to structs *)
type class_struct = {
	sname: string;
	sivars: Sast.variable_decl list;
	(* sparent: Sast.class_decl; *)
	svtable: vtable
}

(* Each struct points to a vtable to handle inheritance *)
and vtable = {
	class_name: string; (* will tell us the name of the struct to create a ptr to *)
	vfuncs: action_decl list;
}

(* C Program consists of structs and function declarations *)
(* Vtables are held by class_struct type *)
and prgrm = class_struct list * Sast.function_decl list