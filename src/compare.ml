(** Comparison function for values *)

open Ast

(** Comparison function for values. First strip attributes, and then
    use built in comparison function. *)
let compare v1 v2 =
  let rec strip_attrs =
    function
    | VRec{pls=[];rls;_} ->
      VRec{at=va; pls=[]; rls=List.map (fun (s,v) -> s,strip_attrs v) rls}
    | VTup{np=0;varr;_}  ->
      VTup{at=va; np=0; varr=Array.map strip_attrs varr}
    | VList{vls;_} -> VList {at=va;vls=List.map strip_attrs vls}

    | VNormal    v -> VNormal  {v with at=va}
    | VUniform   v -> VUniform {v with at=va}
    | VGamma     v -> VGamma   {v with at=va}
    | VExp       v -> VExp     {v with at=va}
    | VBern      v -> VBern    {v with at=va}
    | VBeta      v -> VBeta    {v with at=va}
    | VUnit _      -> VUnit    {at=va}
    | VBool v      -> VBool    {v with at=va}
    | VChar v      -> VChar    {v with at=va}
    | VString v    -> VString  {v with at=va}
    | VInt v       -> VInt     {v with at=va}
    | VFloat v     -> VFloat   {v with at=va}

    | VLam _     | VIf _
    | VCont _    | VFix _    | VRec _
    | VRecProj _ | VTup _    | VTupProj _
    | VCons _    | VUtest _  | VSample _
    | VLogPdf _  | VWeight _ | VResamp _
    | VNot _     | VAnd _    | VOr _
    | VMod _     | VSll _    | VSrl _
    | VSra _     | VLog _    | VAdd _
    | VSub _     | VMul _    | VDiv _
    | VNeg _     | VLt _     | VLeq _
    | VGt _      | VGeq _    | VMatch _
    | VEq _      | VNeq _    | VConcat _   -> failwith "TODO"

  in
  compare (strip_attrs v1) (strip_attrs v2)

