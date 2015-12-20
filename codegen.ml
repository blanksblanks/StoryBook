open Sast
open Cast
open Semantic_analyzer

(* To handle inheritance, make virtual tables for each object type *)
let class_to_vtable (cdecl: Sast.class_decl) =
	{class_name = cdecl.cname; vfuncs = cdecl.cactions}

(* Convert Storybook classes to C Struct types *)
let class_to_struct (cdecl: Sast.class_decl) =
	let vtable = class_to_vtable cdecl in 
	{sname = cdecl.cname; sivars = cdecl.cinstvars; svtable = vtable }

(* Convert classes to structs *)
let sast_to_cast prgm: Cast.prgrm =	
  let (c_dcs, f_dcs) = prgm in
  let cstructs  = List.map (fun c -> class_to_struct c) c_dcs in
  (cstructs, f_dcs)