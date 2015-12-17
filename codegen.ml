open Sast
open Cast
open Semantic_analyzer

(* keep track of amount of objects newed for memory management--freeing*)
let new_count = ref = 0 
let increment_new_count() = new_count := !new_count + 1


let find_new expr = function
	 Sast.Instantiate(v, e) -> increment_new_count()
	| _ -> ""

let class_to_vtable (cdecl: Sast.class_decl) =
	{class_name = cdecl.cname; vfuncs = cdecl.cactions}

let class_to_struct (cdecl: Sast.class_decl) = 
	{sname = cdecl.cname; sivars = cdecl.cinstvars}

let sast_to_cast prgm: Cast.prgrm =	
  let (c_dcs, f_dcs) = prgm in
  (* iterate through functions to find all newed objects *)
  let _ = List.iter (fun f -> List.iter(fun stmt -> find_new stmt) f.funcbody) f_dcs 
  let cstructs  = List.map (fun c -> class_to_struct c) c_dcs in
  let cvtabls = List.map (fun c -> class_to_vtable c) c_dcs in
  (cstructs, cvtabls, f_dcs)