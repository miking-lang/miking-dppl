(** Definitions and operations on the pplcore abstract syntax tree and
    environment. *)

open Pattern

(** Dummy value for labels *)
let no_label = -1

(** Attributes of terms.
    Can easily be extended with more data fields as needed. *)
type tm_attr = {
  label:int;           (* Term labels *)
}

(** Default term attribute *)
let ta = {
  label = no_label;
}

(** Attributes of values. *)
type val_attr = {
  stoch:bool;          (* If this value is the result of some
                          stochastic computation *)
}

(** Default value attribute *)
let va = {
  stoch = false;
}

(** Attributes of variable-related terms (lambdas and vars) *)
type var_attr = {
  var_label:int;       (* Variable labels *)
}

(** Default var attribute *)
let xa = {
  var_label = no_label;
}

(** Lambda calculus terms *)
type term =

  (* Variables *)
  | TVar     of { at:tm_attr; vat:var_attr; x:string; i:int }

  (* Applications *)
  | TApp     of { at:tm_attr; t1:term; t2:term }

  (* Lambdas TODO Add separate term for continuations *)
  | TLam     of { at:tm_attr; vat:var_attr; cont:bool; x:string; t1:term }
  | TIf      of { at:tm_attr; t1:term; t2:term }
  | TMatch   of { at:tm_attr; cls:(pat * term) list } (* TODO match var_attr *)

  (* Value terms.  *)
  | TVal     of { at:tm_attr; v:value }

(** Value terms *)
and value =

  (* Closures *)
  | VClos      of { at:val_attr; vat:var_attr; cont:bool;
                    x:string; t1:term; env:env; stoch_ctrl:bool }
  | VClosIf    of { at:val_attr; t1:term; t2:term; env:env }
  | VClosMatch of { at:val_attr; cls:(pat * term) list; env:env }

  (* Fixed-point combinator (not really needed since untyped) *)
  | VFix       of { at:val_attr }

  (* Record construction and record projection. Use linear search for
     projection. Should we use hashtable for larger records? *)
  | VRec       of { at:val_attr; pls:string list; rls:(string * value) list }
  | VRecProj   of { at:val_attr; k:string }

  (* Tuples and tuple projection. Uses O(1) array lookup for projection *)
  | VTup       of { at:val_attr; np:int; varr:value array }
  | VTupProj   of { at:val_attr; i:int }

  (* Lists and list concatenation *)
  | VList      of { at:val_attr; vls:value list }
  | VCons      of { at:val_attr; v1:value option }

  (* Construct for performing unit tests *)
  | VUtest     of { at:val_attr; pos:Lexing.position; v1:value option }

  (* Probability distributions *)
  | VNormal    of { at:val_attr; mu:float option; sigma:float option }
  | VUniform   of { at:val_attr; a:float option; b:float option }
  | VGamma     of { at:val_attr; a:float option; b:float option }
  | VExp       of { at:val_attr; lam:float option }
  | VBern      of { at:val_attr; p:float option }

  (* Functions operating on probability distributions *)
  | VSample    of { at:val_attr }
  | VLogPdf    of { at:val_attr; v1:value option }

  (* Weighting of executions *)
  | VWeight    of { at:val_attr }

  (* Resample checkpoint for SMC inference (natively in CPS form) *)
  | VResamp    of { at:val_attr; dyn:bool; cont:value option; }

  (* Unit constant *)
  | VUnit      of { at:val_attr }

  (* Boolean constant and operations *)
  | VBool      of { at:val_attr; b:bool }
  | VNot       of { at:val_attr }
  | VAnd       of { at:val_attr; b1:bool option }
  | VOr        of { at:val_attr; b1:bool option }

  (* Character constant and operations *)
  | VChar      of { at:val_attr; c:char }

  (* String constant and operations *)
  | VString    of { at:val_attr; s:string }

  (* Integer constant and operations *)
  | VInt       of { at:val_attr; i:int }
  | VMod       of { at:val_attr; i1:int option }
  | VSll       of { at:val_attr; i1:int option }
  | VSrl       of { at:val_attr; i1:int option }
  | VSra       of { at:val_attr; i1:int option }

  (* Floating-point number constant and operations *)
  | VFloat     of { at:val_attr; f:float }
  | VLog       of { at:val_attr }

  (* Polymorphic integer/floating-point functions *)
  | VAdd       of { at:val_attr; v1:value option }
  | VSub       of { at:val_attr; v1:value option }
  | VMul       of { at:val_attr; v1:value option }
  | VDiv       of { at:val_attr; v1:value option }
  | VNeg       of { at:val_attr }
  | VLt        of { at:val_attr; v1:value option }
  | VLeq       of { at:val_attr; v1:value option }
  | VGt        of { at:val_attr; v1:value option }
  | VGeq       of { at:val_attr; v1:value option }

  (* Polymorphic functions *)
  | VEq        of { at:val_attr; v1:value option }
  | VNeq       of { at:val_attr; v1:value option }

  (* Polymorphic List/String functions *)
  | VConcat    of { at:val_attr; v1:value option }

(** Evaluation environment. Can not be of type value list because of fixpoint
    operator. *)
and env = term list

(** Returns the number of expected arguments for values *)
let arity c = match c with

  | VClos _ | VClosIf _ | VClosMatch _ -> 1

  | VFix _       -> 1

  | VRec{pls;_} -> List.length pls
  | VRecProj _  -> 1

  | VTup{np;_}  -> np
  | VTupProj _  -> 1

  | VList _     -> 0

  | VCons{v1=None;_} -> 2 | VCons{v1=Some _;_} -> 1

  | VUtest{v1=None;_} -> 2 | VUtest{v1=Some _;_} -> 1

  | VNormal{mu=None;  sigma=None;_}   -> 2
  | VNormal{mu=Some _;sigma=None;_}   -> 1
  | VNormal{mu=Some _;sigma=Some _;_} -> 0
  | VNormal _                         -> failwith "Should not happen"
  | VUniform{a=None;  b=None;_}       -> 2
  | VUniform{a=Some _;b=None;_}       -> 1
  | VUniform{a=Some _;b=Some _;_}     -> 0
  | VUniform _                        -> failwith "Should not happen"
  | VGamma{a=None;  b=None;_}         -> 2
  | VGamma{a=Some _;b=None;_}         -> 1
  | VGamma{a=Some _;b=Some _;_}       -> 0
  | VGamma _                          -> failwith "Should not happen"
  | VExp{lam=None;_}                  -> 1
  | VExp _                            -> 1
  | VBern{p=None;_}                   -> 1
  | VBern _                           -> 1

  | VSample _ -> 1

  | VLogPdf{v1=None;_} -> 2 | VLogPdf{v1=Some _;_} -> 1

  | VWeight _ -> 1

  | VResamp _ -> failwith "Resample arity should not be checked"

  | VUnit _ -> 0

  | VBool _    -> 0
  | VNot _     -> 1
  | VAnd{b1=None;_} -> 2  | VAnd{b1=Some _;_} -> 1
  | VOr{b1=None;_}  -> 2  | VOr{b1=Some _;_}  -> 1

  | VChar _ -> 0

  | VString _ -> 0

  | VInt _       -> 0
  | VMod{i1=None;_} -> 2  | VMod{i1=Some _;_} -> 1
  | VSll{i1=None;_} -> 2  | VSll{i1=Some _;_} -> 1
  | VSrl{i1=None;_} -> 2  | VSrl{i1=Some _;_} -> 1
  | VSra{i1=None;_} -> 2  | VSra{i1=Some _;_} -> 1

  | VFloat _ -> 0
  | VLog _   -> 1

  | VAdd{v1=None;_} -> 2  | VAdd _ -> 1
  | VSub{v1=None;_} -> 2  | VSub _ -> 1
  | VMul{v1=None;_} -> 2  | VMul _ -> 1
  | VDiv{v1=None;_} -> 2  | VDiv _ -> 1
  | VNeg   _        -> 1
  | VLt{v1=None;_}  -> 2  | VLt _  -> 1
  | VLeq{v1=None;_} -> 2  | VLeq _ -> 1
  | VGt{v1=None;_}  -> 2  | VGt _  -> 1
  | VGeq{v1=None;_} -> 2  | VGeq _ -> 1

  | VEq{v1=None;_}  -> 2  | VEq{v1=Some _;_}  -> 1
  | VNeq{v1=None;_} -> 2  | VNeq{v1=Some _;_} -> 1

  | VConcat{v1=None;_} -> 2 | VConcat{v1=Some _;_} -> 1

(** Returns the attribute of a value *)
let val_attr = function
  | VCons{at;_}
  | VClos{at;_}   | VClosIf{at;_}  | VClosMatch{at;_}
  | VFix{at;_}    | VRec{at;_}     | VRecProj{at;_}
  | VTup{at;_}    | VTupProj{at;_} | VList{at;_}
  | VUtest{at;_}  | VNormal{at;_}  | VUniform{at;_}
  | VGamma{at;_}  | VExp{at;_}     | VBern{at;_}
  | VSample{at;_} | VLogPdf{at;_}  | VWeight{at;_}
  | VResamp{at;_} | VUnit{at;_}    | VBool{at;_}
  | VNot{at;_}    | VAnd{at;_}     | VOr{at;_}
  | VChar{at;_}   | VString{at;_}  | VInt{at;_}
  | VMod{at;_}    | VSll{at;_}     | VSrl{at;_}
  | VSra{at;_}    | VFloat{at;_}   | VLog{at;_}
  | VAdd{at;_}    | VSub{at;_}     | VMul{at;_}
  | VDiv{at;_}    | VNeg{at;_}     | VLt{at;_}
  | VLeq{at;_}    | VGt{at;_}      | VGeq{at;_}
  | VEq{at;_}     | VNeq{at;_}     | VConcat{at;_} -> at

(** Returns the attribute of a term *)
let tm_attr = function
  | TVar{at;_} | TApp{at;_}
  | TLam{at;_} | TIf{at;_} | TMatch{at;_}
  | TVal{at;_} -> at

(** Returns the label of a term *)
let tm_label tm = match tm_attr tm with {label;_} -> label

(** Change the value attribute of a given value *)
let update_val_attr attr = function
  | VClos      v -> VClos      {v with at=attr}
  | VClosIf    v -> VClosIf    {v with at=attr}
  | VClosMatch v -> VClosMatch {v with at=attr}
  | VFix       _ -> VFix       {at=attr}
  | VRec       v -> VRec       {v with at=attr}
  | VRecProj   v -> VRecProj   {v with at=attr}
  | VTup       v -> VTup       {v with at=attr}
  | VTupProj   v -> VTupProj   {v with at=attr}
  | VList      v -> VList      {v with at=attr}
  | VCons      v -> VCons      {v with at=attr}
  | VUtest     v -> VUtest     {v with at=attr}
  | VNormal    v -> VNormal    {v with at=attr}
  | VUniform   v -> VUniform   {v with at=attr}
  | VGamma     v -> VGamma     {v with at=attr}
  | VExp       v -> VExp       {v with at=attr}
  | VBern      v -> VBern      {v with at=attr}
  | VSample    _ -> VSample    {at=attr}
  | VLogPdf    v -> VLogPdf    {v with at=attr}
  | VWeight    _ -> VWeight    {at=attr}
  | VResamp    v -> VResamp    {v with at=attr}
  | VUnit      _ -> VUnit      {at=attr}
  | VBool      v -> VBool      {v with at=attr}
  | VNot       _ -> VNot       {at=attr}
  | VAnd       v -> VAnd       {v with at=attr}
  | VOr        v -> VOr        {v with at=attr}
  | VChar      v -> VChar      {v with at=attr}
  | VString    v -> VString    {v with at=attr}
  | VInt       v -> VInt       {v with at=attr}
  | VMod       v -> VMod       {v with at=attr}
  | VSll       v -> VSll       {v with at=attr}
  | VSrl       v -> VSrl       {v with at=attr}
  | VSra       v -> VSra       {v with at=attr}
  | VFloat     v -> VFloat     {v with at=attr}
  | VLog       _ -> VLog       {at=attr}
  | VAdd       v -> VAdd       {v with at=attr}
  | VSub       v -> VSub       {v with at=attr}
  | VMul       v -> VMul       {v with at=attr}
  | VDiv       v -> VDiv       {v with at=attr}
  | VNeg       _ -> VNeg       {at=attr}
  | VLt        v -> VLt        {v with at=attr}
  | VLeq       v -> VLeq       {v with at=attr}
  | VGt        v -> VGt        {v with at=attr}
  | VGeq       v -> VGeq       {v with at=attr}
  | VEq        v -> VEq        {v with at=attr}
  | VNeq       v -> VNeq       {v with at=attr}
  | VConcat    v -> VConcat    {v with at=attr}

(** Change the term attribute of a given term *)
let update_tm_attr attr = function
  | TVar   t -> TVar   {t with at=attr}
  | TLam   t -> TLam   {t with at=attr}
  | TApp   t -> TApp   {t with at=attr}
  | TVal   t -> TVal   {t with at=attr}
  | TIf    t -> TIf    {t with at=attr}
  | TMatch t -> TMatch {t with at=attr}

(** Make a value stochastic if cond is true. If the value is already
    stochastic, do nothing *)
let set_stoch cond v =
  let {stoch;_} = val_attr v in
  update_val_attr {stoch=stoch || cond} v

(** Reference used for genvar *)
let nextvar = ref 0

(** Generate fresh variable names (used for CPS transformation).  Avoids
    clashes by using $ as first char (not allowed in lexer for vars).  Takes a
    debruijn index as argument (for idfun). *)
let genvar i =
  let res = !nextvar in
  let str = "$" ^ string_of_int res in
  nextvar := res + 1;
  (str,TVar{at=ta; vat=xa; x=str; i=i})

(** Similar to genvar, but specify your own variable name *)
let makevar str i = (str,TVar{at=ta; vat=xa; x=str; i=i})

(** Shorthands *)
let nop = TVal{at=ta;v=VUnit{at=va}}
let fix = TVal{at=ta;v=VFix{at=va}}

(** The identity function (with proper debruijn index) as a continuation. *)
let idfun = let var,var' = genvar 0 in
  TLam{at=ta;vat=xa;cont=true;x=var;t1=var'}

(** Convenience function for creating a sequence of two tms *)
let seq t1 t2 = TApp{at=ta;
                     t1=TLam{at=ta;vat=xa;cont=false;x="_";t1=t2};
                     t2=t1}

(** Function for wrapping a value in a tm *)
let tm_of_val c = TVal{at=ta;v=c}

(** Used for indicating uninitialized debruijn indices *)
let noidx = -1

