(** Definitions and operations on the pplcore abstract syntax tree and
    environment. *)

open Pattern

(** Attributes of terms and values.
    Can easily be extended with more data fields as needed. *)
type attr =
  {
    label:int;           (* Term labels *)
    var_label:int;       (* Variable labels *)
    pos:Lexing.position; (* Term position in source code *)
    stoch:bool;          (* Whether or not the term is the result of some
                            stochastic computation*)
  }

(** Dummy value for labels *)
let no_label = -1

(** Default attribute with dummy values *)
let na =
  {
    label = no_label;
    var_label = no_label;
    pos = Lexing.dummy_pos;
    stoch = false;
  }

(** Lambda calculus terms *)
type term =

  (* Variables *)
  | TVar     of attr * string * int

  (* Applications *)
  | TApp     of attr * term * term

  (* Lambdas (All can be encoded as TmMatch) *)
  | TLam     of attr * string * term
  | TIf      of attr * term * term
  | TMatch   of attr * (pat * term) list

  (* Value terms. The attributes are stored in the value itself. *)
  | TVal     of value

(** Value terms *)
and value =

  (* Closures *)
  | VClos      of attr * string * term * env
  | VClosIf    of attr * term * term * env
  | VClosMatch of attr * (pat * term) list * env

  (* Fixed-point combinator (not really needed since untyped) *)
  | VFix       of attr

  (* Record construction and record projection. Use linear search for
     projection. Should we use hashtable for larger records?
     TODO Split into value and constructor*)
  | VRec       of attr * string list * (string * value) list
  | VRecProj   of attr * string

  (* Tuples and tuple projection. Uses O(1) array lookup for projection
     TODO Split into value and constructor *)
  | VTup       of attr * int * value array
  | VTupProj   of attr * int

  (* Lists and list concatenation *)
  | VList      of attr * value list
  | VCons      of attr * value option

  (* Construct for performing unit tests *)
  | VUtest     of attr * value option

  (* Probability distributions *)
  | VNormal    of attr * float option * float option
  | VUniform   of attr * float option * float option
  | VGamma     of attr * float option * float option
  | VExp       of attr * float option
  | VBern      of attr * float option

  (* Functions operating on probability distributions *)
  | VSample    of attr
  | VLogPdf    of attr * value option

  (* Weighting of executions *)
  | VWeight    of attr

  (* Resample checkpoint for SMC inference (natively in CPS form) *)
  | VResamp    of attr * value option * bool option

  (* Unit constant *)
  | VUnit      of attr

  (* Boolean constant and operations *)
  | VBool      of attr * bool
  | VNot       of attr
  | VAnd       of attr * bool option
  | VOr        of attr * bool option

  (* Character constant and operations *)
  | VChar      of attr * char

  (* String constant and operations *)
  | VString    of attr * string

  (* Integer constant and operations *)
  | VInt       of attr * int
  | VMod       of attr * int option
  | VSll       of attr * int option
  | VSrl       of attr * int option
  | VSra       of attr * int option

  (* Floating-point number constant and operations *)
  | VFloat     of attr * float
  | VLog       of attr

  (* Polymorphic integer/floating-point functions *)
  | VAdd       of attr * value option
  | VSub       of attr * value option
  | VMul       of attr * value option
  | VDiv       of attr * value option
  | VNeg       of attr
  | VLt        of attr * value option
  | VLeq       of attr * value option
  | VGt        of attr * value option
  | VGeq       of attr * value option

  (* Polymorphic functions *)
  | VEq        of attr * value option
  | VNeq       of attr * value option

  (* Polymorphic List/String functions *)
  | VConcat    of attr * value option

(** Evaluation environment. Can not be of type value list because of fixpoint
    operator. *)
and env = term list

(** Returns the number of expected arguments for values *)
let arity c = match c with

  | VClos _ | VClosIf _ | VClosMatch _ -> 1

  | VFix _       -> 1

  | VRec(_,strs,_) -> List.length strs
  | VRecProj _   -> 1

  | VTup(_,i,_)    -> i
  | VTupProj _   -> 1

  | VList _      -> 0

  | VCons(_,None) -> 2 | VCons(_,Some _) -> 1

  | VUtest(_,None) -> 2 | VUtest(_,Some _) -> 1

  | VNormal(_,None,None)       -> 2
  | VNormal(_,Some _,None)     -> 1
  | VNormal(_,Some _,Some _)   -> 0
  | VNormal _                -> failwith "Should not happen"
  | VUniform(_,None,None)      -> 2
  | VUniform(_,Some _, None)   -> 1
  | VUniform(_,Some _, Some _) -> 0
  | VUniform _               -> failwith "Should not happen"
  | VGamma(_,None,None)        -> 2
  | VGamma(_,Some _, None)     -> 1
  | VGamma(_,Some _, Some _)   -> 0
  | VGamma _                 -> failwith "Should not happen"
  | VExp(_,None)               -> 1
  | VExp _                   -> 1
  | VBern(_,None)              -> 1
  | VBern _                  -> 1

  | VSample _ -> 1

  | VLogPdf(_,None) -> 2 | VLogPdf(_,Some _) -> 1

  | VWeight _ -> 1

  | VResamp _ -> failwith "Resample arity should not be checked"

  | VUnit _ -> 0

  | VBool _    -> 0
  | VNot _     -> 1
  | VAnd(_,None) -> 2  | VAnd(_,Some _) -> 1
  | VOr(_,None)  -> 2  | VOr(_,Some _)  -> 1

  | VChar _ -> 0

  | VString _ -> 0

  | VInt _       -> 0
  | VMod(_,None) -> 2  | VMod(_,Some _) -> 1
  | VSll(_,None) -> 2  | VSll(_,Some _) -> 1
  | VSrl(_,None) -> 2  | VSrl(_,Some _) -> 1
  | VSra(_,None) -> 2  | VSra(_,Some _) -> 1

  | VFloat _ -> 0
  | VLog _   -> 1

  | VAdd(_,None) -> 2  | VAdd _ -> 1
  | VSub(_,None) -> 2  | VSub _ -> 1
  | VMul(_,None) -> 2  | VMul _ -> 1
  | VDiv(_,None) -> 2  | VDiv _ -> 1
  | VNeg   _     -> 1
  | VLt(_,None)  -> 2  | VLt _  -> 1
  | VLeq(_,None) -> 2  | VLeq _ -> 1
  | VGt(_,None)  -> 2  | VGt _  -> 1
  | VGeq(_,None) -> 2  | VGeq _ -> 1

  | VEq(_,None)  -> 2  | VEq(_,Some _)  -> 1
  | VNeq(_,None) -> 2  | VNeq(_,Some _) -> 1

  | VConcat(_,None) -> 2 | VConcat(_,Some _) -> 1

(** Comparison function for values. First strip attributes, and then
    use built in comparison function. *)
let compare v1 v2 =
  let rec strip_attrs = function
    | VRec(_,[],els)    ->
      VRec(na,[],List.map (fun (s,v) -> s,strip_attrs v) els)
    | VTup(_,0,arr)     -> VTup(na,0,Array.map strip_attrs arr)
    | VList(_,ls)       -> VList(na,List.map strip_attrs ls)
    | VNormal(_,f1,f2)  -> VNormal(na,f1,f2)
    | VUniform(_,f1,f2) -> VUniform(na,f1,f2)
    | VGamma(_,f1,f2)   -> VGamma(na,f1,f2)
    | VExp(_,f)         -> VExp(na,f)
    | VBern(_,f)        -> VBern(na,f)
    | VUnit(_)          -> VUnit(na)
    | VBool(_,b)        -> VBool(na,b)
    | VChar(_,c)        -> VChar(na,c)
    | VString(_,s)      -> VString(na,s)
    | VInt(_,i)         -> VInt(na,i)
    | VFloat(_,f)       -> VFloat(na,f)
    | v                 -> v
  in compare (strip_attrs v1) (strip_attrs v2)

(** Returns the attribute of a value *)
let val_attr = function
  | VCons(a,_)
  | VClos(a,_,_,_) | VClosIf(a,_,_,_) | VClosMatch(a,_,_)
  | VFix(a)        | VRec(a,_,_)      | VRecProj(a,_)
  | VTup(a,_,_)    | VTupProj(a,_)    | VList(a,_)
  | VUtest(a,_)    | VNormal(a,_,_)   | VUniform(a,_,_)
  | VGamma(a,_,_)  | VExp(a,_)        | VBern(a,_)
  | VSample(a)     | VLogPdf(a,_)     | VWeight(a)
  | VResamp(a,_,_) | VUnit(a)         | VBool(a,_)
  | VNot(a)        | VAnd(a,_)        | VOr(a,_)
  | VChar(a,_)     | VString(a,_)     | VInt(a,_)
  | VMod(a,_)      | VSll(a,_)        | VSrl(a,_)
  | VSra(a,_)      | VFloat(a,_)      | VLog(a)
  | VAdd(a,_)      | VSub(a,_)        | VMul(a,_)
  | VDiv(a,_)      | VNeg(a)          | VLt(a,_)
  | VLeq(a,_)      | VGt(a,_)         | VGeq(a,_)
  | VEq(a,_)       | VNeq(a,_)        | VConcat(a,_) -> a

(** Returns the attribute of a term *)
let tm_attr = function
  | TVar(a,_,_) | TApp(a,_,_)
  | TLam(a,_,_) | TIf(a,_,_) | TMatch(a,_) -> a
  | TVal(v) -> val_attr v

(** Returns the label of a term *)
let tm_label tm = match tm_attr tm with {label;_} -> label

(** Change the attribute of a given value *)
let update_attr_val attr = function
  | VClos      (_,s,t,env)   -> VClos      (attr,s,t,env)
  | VClosIf    (_,t1,t2,env) -> VClosIf    (attr,t1,t2,env)
  | VClosMatch (_,cls,env)   -> VClosMatch (attr,cls,env)
  | VFix       (_)           -> VFix       (attr)
  | VRec       (_,sls,els)   -> VRec       (attr,sls,els)
  | VRecProj   (_,s)         -> VRecProj   (attr,s)
  | VTup       (_,i,arr)     -> VTup       (attr,i,arr)
  | VTupProj   (_,i)         -> VTupProj   (attr,i)
  | VList      (_,ls)        -> VList      (attr,ls)
  | VCons      (_,v)         -> VCons      (attr,v)
  | VUtest     (_,v)         -> VUtest     (attr,v)
  | VNormal    (_,f1,f2)     -> VNormal    (attr,f1,f2)
  | VUniform   (_,f1,f2)     -> VUniform   (attr,f1,f2)
  | VGamma     (_,f1,f2)     -> VGamma     (attr,f1,f2)
  | VExp       (_,f)         -> VExp       (attr,f)
  | VBern      (_,f)         -> VBern      (attr,f)
  | VSample    (_)           -> VSample    (attr)
  | VLogPdf    (_,v)         -> VLogPdf    (attr,v)
  | VWeight    (_)           -> VWeight    (attr)
  | VResamp    (_,v,b)       -> VResamp    (attr,v,b)
  | VUnit      (_)           -> VUnit      (attr)
  | VBool      (_,b)         -> VBool      (attr,b)
  | VNot       (_)           -> VNot       (attr)
  | VAnd       (_,b)         -> VAnd       (attr,b)
  | VOr        (_,b)         -> VOr        (attr,b)
  | VChar      (_,c)         -> VChar      (attr,c)
  | VString    (_,s)         -> VString    (attr,s)
  | VInt       (_,i)         -> VInt       (attr,i)
  | VMod       (_,i)         -> VMod       (attr,i)
  | VSll       (_,i)         -> VSll       (attr,i)
  | VSrl       (_,i)         -> VSrl       (attr,i)
  | VSra       (_,i)         -> VSra       (attr,i)
  | VFloat     (_,f)         -> VFloat     (attr,f)
  | VLog       (_)           -> VLog       (attr)
  | VAdd       (_,v)         -> VAdd       (attr,v)
  | VSub       (_,v)         -> VSub       (attr,v)
  | VMul       (_,v)         -> VMul       (attr,v)
  | VDiv       (_,v)         -> VDiv       (attr,v)
  | VNeg       (_)           -> VNeg       (attr)
  | VLt        (_,v)         -> VLt        (attr,v)
  | VLeq       (_,v)         -> VLeq       (attr,v)
  | VGt        (_,v)         -> VGt        (attr,v)
  | VGeq       (_,v)         -> VGeq       (attr,v)
  | VEq        (_,v)         -> VEq        (attr,v)
  | VNeq       (_,v)         -> VNeq       (attr,v)
  | VConcat    (_,v)         -> VConcat    (attr,v)

(** Change the attribute of a given term *)
let update_attr_tm attr = function
  | TVar   (_,x,i)   -> TVar   (attr,x,i)
  | TLam   (_,x,t1)  -> TLam   (attr,x,t1)
  | TApp   (_,t1,t2) -> TApp   (attr,t1,t2)
  | TVal   (v)       -> TVal   (update_attr_val attr v)
  | TIf    (_,t1,t2) -> TIf    (attr,t1,t2)
  | TMatch (_,cases) -> TMatch (attr,cases)

(** Make a value stochastic *)
let set_stoch stoch v = update_attr_val {(val_attr v) with stoch=stoch} v

(** Reference used for genvar *)
let nextvar = ref 0

(** Generate fresh variable names (used for CPS transformation).  Avoids
    clashes by using $ as first char (not allowed in lexer for vars).  Takes a
    debruijn index as argument (for idfun). *)
let genvar i =
  let res = !nextvar in
  let str = "$" ^ string_of_int res in
  nextvar := res + 1;
  (str,TVar(na,str,i))

(** Similar to genvar, but specify your own variable name *)
let makevar str i = (str,TVar(na,str,i))

(** Shorthands *)
let nop = TVal(VUnit na)
let fix = TVal(VFix na)

(** The identity function (with proper debruijn index) as a tm. *)
let idfun = let var,var' = genvar 0 in TLam(na,var,var')

(** Convenience function for creating a sequence of two tms *)
let seq t1 t2 = TApp(na,TLam(na,"_",t2),t1)

(** Function for wrapping a value in a tm *)
let tm_of_val c = TVal(c)

(** Used for indicating uninitialized debruijn indices *)
let noidx = -1

