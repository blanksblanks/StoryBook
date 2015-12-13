open Sast
open Cast
open Semantic_analyzer

let class_to_vtable (cdecl: Sast.class_decl) =
	{class_name = cdecl.cname; vfuncs = cdecl.cactions}

let class_to_struct (cdecl: Sast.class_decl) = 
	{sname = cdecl.cname; sivars = cdecl.cinstvars}

let sast_to_cast prgm: Sast.program =	
  let (c_dcs, f_dcs) = prgm in
  let _  = List.map (fun c -> class_to_struct c) c_dcs in
  let _ = List.map (fun c -> class_to_vtable c) c_dcs in
  (c_dcs, f_dcs)