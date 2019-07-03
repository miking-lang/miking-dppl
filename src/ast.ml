(** Definitions and operations on the pplcore abstract syntax tree and
    environment. *)

open Pattern

(** Attributes of terms.  Can easily be extended with more data fields as
    needed. *)
type tm_attr = {

  (* Term labels *)
  label:int;

}

(** Default term attribute *)
let ta = {
  label = -1;
}

(** Attributes of values. *)
type val_attr = {

  (* If this value is the result of some stochastic computation *)
  stoch:bool;

}

(** Default value attribute *)
let va = {
  stoch = false;
}

(** Attributes of variable-related terms (lambdas and vars) *)
type var_attr = {

  (* Variable labels *)
  var_label:int;

}

(** Default var attribute *)
let xa = {
  var_label = -1;
}

(** Lambda calculus terms *)
type term =
  | T of { at:tm_attr; t:term' }

and term' =

  (* Variables *)
  | TVar     of { xat:var_attr; x:string; i:int }

  (* Applications *)
  | TApp     of { t1:term; t2:term }

  (* Lambdas *)
  | TLam     of { xat:var_attr; x:string; t1:term }
  | TIf      of { t1:term; t2:term }
  | TMatch   of { cls:(pat * term) list } (* TODO add var_attr *)

  (* Continuations *)
  | TCont    of { x:string; t1:term }

  (* Value terms.  *)
  | TVal     of { v:value }

(** Value terms *)
and value =
  | V of { at:val_attr; v:value' }

and value' =

  (* Closures *)
  | VLam       of { x:string; t1:term; env:env; }
  | VIf        of { t1:term; t2:term; env:env }
  | VMatch     of { cls:(pat * term) list; env:env }
  | VCont      of { x:string; t1:term; stoch_ctrl:bool; env:env }

  (* Fixed-point combinator (not really needed since untyped) *)
  | VFix

  (* Record construction and record projection. Use linear search for
     projection. Should we use hashtable for larger records? *)
  | VRec       of { pls:string list; rls:(string * value) list }
  | VRecProj   of { k:string }

  (* Tuples and tuple projection. Uses O(1) array lookup for projection *)
  | VTup       of { np:int; varr:value array }
  | VTupProj   of { i:int }

  (* Lists and list concatenation *)
  | VList      of { vls:value list }
  | VCons      of { v1:value option }

  (* Construct for performing unit tests *)
  | VUtest     of { pos:Lexing.position; v1:value option }

  (* Polymorphic functions *)
  | VEq        of { v1:value option }
  | VNeq       of { v1:value option }

  (* Polymorphic List/String concatenation function *)
  | VConcat    of { v1:value option }

  (* Probability distributions *)
  | VDist      of { d:dist }

  (* Function for getting log probability densities for probability
     distributions *)
  | VLogPdf    of { v1:value option }

  (* Weighting of executions (natively in CPS form) *)
  | VWeight    of { cont:value option; w:float option }

  (* Sampling from distributions (natively in CPS form) *)
  | VSample    of { cont:value option; d:dist option }

  (* Explicit resampling point for SMC (natively in CPS form) *)
  | VResamp    of { dyn:bool; cont:value option; stoch_ctrl:bool option; }

  (* Unit constant *)
  | VUnit

  (* Boolean constant and operations *)
  | VBool      of { b:bool }
  | VNot
  | VAnd       of { b1:bool option }
  | VOr        of { b1:bool option }

  (* Character constant and operations *)
  | VChar      of { c:char }

  (* String constant and operations *)
  | VString    of { s:string }

  (* Integer constant and operations *)
  | VInt       of { i:int }
  | VMod       of { i1:int option }
  | VSll       of { i1:int option }
  | VSrl       of { i1:int option }
  | VSra       of { i1:int option }

  (* Floating-point number constant and operations *)
  | VFloat     of { f:float }
  | VLog

  (* Polymorphic integer/floating-point functions *)
  | VAdd       of { v1:value option }
  | VSub       of { v1:value option }
  | VMul       of { v1:value option }
  | VDiv       of { v1:value option }
  | VNeg
  | VLt        of { v1:value option }
  | VLeq       of { v1:value option }
  | VGt        of { v1:value option }
  | VGeq       of { v1:value option }

(** Evaluation environment. Can not be of type value list because of fixpoint
    operator. *)
and env = term list

(** Probability distributions *)
and dist =
  | DNormal   of { mu:float option; sigma:float option }
  | DUniform  of { a:float option; b:float option }
  | DGamma    of { a:float option; b:float option }
  | DExp      of { lam:float option }
  | DBern     of { p:float option }
  | DBeta     of { a:float option; b:float option }

(** Returns the number of expected arguments for values *)
let arity (V{v;_}) = match v with
  | VLam  _            -> 1
  | VIf _              -> 1
  | VMatch _           -> 1
  | VCont _            -> 1
  | VFix               -> 1
  | VRec{pls;_}        -> List.length pls
  | VRecProj _         -> 1
  | VTup{np;_}         -> np
  | VTupProj _         -> 1
  | VList _            -> 0
  | VCons{v1=None;_}   -> 2 | VCons{v1=Some _;_}  -> 1
  | VUtest{v1=None;_}  -> 2 | VUtest{v1=Some _;_} -> 1
  | VSample _          -> 1
  | VDist{d;_} ->
    begin match d with
     | DNormal{mu=None;  sigma=None}   -> 2
     | DNormal{mu=Some _;sigma=None}   -> 1
     | DNormal{mu=Some _;sigma=Some _} -> 0
     | DNormal _                       -> failwith "Should not happen"
     | DUniform{a=None;  b=None}       -> 2
     | DUniform{a=Some _;b=None}       -> 1
     | DUniform{a=Some _;b=Some _}     -> 0
     | DUniform _                      -> failwith "Should not happen"
     | DGamma{a=None;  b=None}         -> 2
     | DGamma{a=Some _;b=None}         -> 1
     | DGamma{a=Some _;b=Some _}       -> 0
     | DGamma _                        -> failwith "Should not happen"
     | DExp{lam=None}                  -> 1
     | DExp _                          -> 1
     | DBern{p=None}                   -> 1
     | DBern _                         -> 1
     | DBeta{a=None;  b=None}          -> 2
     | DBeta{a=Some _;b=None}          -> 1
     | DBeta{a=Some _;b=Some _}        -> 0
     | DBeta _                         -> failwith "Should not happen"
    end
  | VLogPdf{v1=None;_} -> 2 | VLogPdf{v1=Some _;_} -> 1
  | VWeight _          -> 1
  | VResamp _          -> failwith "Resample arity should never be checked"
  | VUnit              -> 0
  | VBool _            -> 0
  | VNot               -> 1
  | VAnd{b1=None;_}    -> 2 | VAnd{b1=Some _;_} -> 1
  | VOr{b1=None;_}     -> 2 | VOr{b1=Some _;_}  -> 1
  | VChar _            -> 0
  | VString _          -> 0
  | VInt _             -> 0
  | VMod{i1=None;_}    -> 2 | VMod{i1=Some _;_} -> 1
  | VSll{i1=None;_}    -> 2 | VSll{i1=Some _;_} -> 1
  | VSrl{i1=None;_}    -> 2 | VSrl{i1=Some _;_} -> 1
  | VSra{i1=None;_}    -> 2 | VSra{i1=Some _;_} -> 1
  | VFloat _           -> 0
  | VLog               -> 1
  | VAdd{v1=None;_}    -> 2 | VAdd _ -> 1
  | VSub{v1=None;_}    -> 2 | VSub _ -> 1
  | VMul{v1=None;_}    -> 2 | VMul _ -> 1
  | VDiv{v1=None;_}    -> 2 | VDiv _ -> 1
  | VNeg               -> 1
  | VLt{v1=None;_}     -> 2 | VLt _                -> 1
  | VLeq{v1=None;_}    -> 2 | VLeq _               -> 1
  | VGt{v1=None;_}     -> 2 | VGt _                -> 1
  | VGeq{v1=None;_}    -> 2 | VGeq _               -> 1
  | VEq{v1=None;_}     -> 2 | VEq{v1=Some _;_}     -> 1
  | VNeq{v1=None;_}    -> 2 | VNeq{v1=Some _;_}    -> 1
  | VConcat{v1=None;_} -> 2 | VConcat{v1=Some _;_} -> 1

(** Returns the attribute of a term *)
let tm_attr = function T{at;_} -> at

(** Returns the attribute of a value *)
let val_attr = function V{at;_} -> at

(** Returns the label of a term *)
let tm_label = function T{at={label;_};_} -> label

(** Change the term attribute of a given term *)
let update_tm_attr attr = function T t -> T{t with at=attr}

(** Change the value attribute of a given value *)
let update_val_attr attr = function V v -> V{v with at=attr}

(** Shorthands *)
let nop = T{at=ta;t=TVal{v=V{at=va;v=VUnit}}}
let fix = T{at=ta;t=TVal{v=V{at=va;v=VFix}}}

(** Convenience functions for making common terms *)
let mkapp ~t1 ~t2  = T{at=ta;t=TApp{t1;t2}}
let mkapp' ~t1 ~t2 = T{at=ta;t=TApp{t1=T{at=ta;t=t1};t2=T{at=ta;t=t2}}}
let mklam ~x ~t1   = T{at=ta;t=TLam{xat=xa;x=x;t1}}
let mklam' ~x ~t1  = T{at=ta;t=TLam{xat=xa;x=x;t1=T{at=ta;t=t1}}}
let mkif ~t1 ~t2   = T{at=ta;t=TIf{t1;t2}}
let mkmatch ~cls   = T{at=ta;t=TMatch{cls}}
let mkcont ~x ~t1  = T{at=ta;t=TCont{x=x;t1}}
let mkcont' ~x ~t1 = T{at=ta;t=TCont{x=x;t1=T{at=ta;t=t1}}}
let mkvar ~x ~i    = T{at=ta;t=TVar{xat=xa;x=x;i=i}}

(** Convenient conversion functions *)
let tm_of_tm' t = T{at=ta;t}
let tm_of_val v = T{at=ta;t=TVal{v=v}}
let tm_of_val' v = T{at=ta;t=TVal{v=V{at=va;v}}}
let tm_of_dist d = tm_of_val' (VDist{d})
let val_of_val' v = V{at=va;v=v}

(** Used for indicating uninitialized debruijn indices *)
let noidx = -1

