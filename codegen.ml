open Sast
open Cast
open Semantic_analyzer

let sast_to_cast prgm: Sast.program =
  let (v_dcs, f_dcs) = prgm in
  ([], f_dcs)