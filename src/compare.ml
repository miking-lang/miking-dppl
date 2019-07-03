(** Comparison function for values *)

open Ast

(** Comparison function for values. First strip attributes, and then
    use built in comparison function. *)
let compare v1 v2 =
  let rec strip_attrs (V{v;_}) =
    let v = match v with
      | VRec{pls=[];rls;_} ->
        VRec{pls=[]; rls=List.map (fun (s,v) -> s,strip_attrs v) rls}
      | VTup{np=0;varr;_}  ->
        VTup{np=0; varr=Array.map strip_attrs varr}
      | VList{vls;_} -> VList{vls=List.map strip_attrs vls}

      | VUnit     | VBool _    | VChar _
      | VString _ | VInt _     | VFloat _ -> v

      | VDist{d} ->
        begin match d with
          | DNormal _ | DUniform _ | DGamma _
          | DExp _    | DBern _    | DBeta _ -> v
        end

      | VLam _     | VIf _
      | VCont _    | VFix      | VRec _
      | VRecProj _ | VTup _    | VTupProj _
      | VCons _    | VUtest _  | VSample _
      | VLogPdf _  | VWeight _ | VResamp _
      | VNot       | VAnd _    | VOr _
      | VMod _     | VSll _    | VSrl _
      | VSra _     | VLog      | VAdd _
      | VSub _     | VMul _    | VDiv _
      | VNeg       | VLt _     | VLeq _
      | VGt _      | VGeq _    | VMatch _
      | VEq _      | VNeq _    | VConcat _  -> failwith "TODO"
    in
    V{at=va;v}

  in
  compare (strip_attrs v1) (strip_attrs v2)

