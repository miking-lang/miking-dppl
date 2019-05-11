(** Definitions and operations on the pplcore abstract syntax tree and
    environment. *)

open Pattern

(** Attributes of terms TODO Values also need to have attributes.
    Can easily be extended with more data fields as needed. *)
type attr =
  {
    label:int; (* Term labels *)
    var_label:int; (* Variable labels *)
    pos:Lexing.position; (* Term position in source code *)
    stoch:bool; (* Whether or not the term is the result of some stochastic
                   computation*)
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

  (* Lambdas (All can be encoded as TmMatch really) *)
  | TLam     of attr * string * term
  | TIf      of attr * term * term
  | TMatch   of attr * (pat * term) list

  (* Value terms *)
  | TVal     of attr * value

(** Evaluation environment. Can not be of type value list because of fixpoint
    operator. *)
and env = term list

(** Value terms *)
and value =

  (* Closures *)
  | VClos      of string * term * env
  | VClosIf    of term * term * env
  | VClosMatch of (pat * term) list * env

  (* Fixed-point combinator (not really needed since untyped) *)
  | VFix

  (* Record construction and record projection. Use linear search for
     projection.  Should we use hashtable for larger records? *)
  | VRec     of string list * (string * value) list
  | VRecProj of string

  (* Tuples and tuple projection. Uses O(1) array lookup for projection *)
  | VTup     of int * value array
  | VTupProj of int

  (* Lists. Works both as a cons function and a list value *)
  | VList    of value list

  (* Construct for performing unit tests *)
  | VUtest   of value option

  (* Probability distributions *)
  | VNormal  of float option * float option
  | VUniform of float option * float option
  | VGamma   of float option * float option
  | VExp     of float option
  | VBern    of float option

  (* Functions operating on probability distributions *)
  | VSample
  | VLogPdf  of value option

  (* Weighting of executions *)
  | VWeight

  (* Resample checkpoint for SMC inference (natively in CPS form) *)
  | VResamp  of value option * bool option

  (* Unit constant *)
  | VUnit

  (* Boolean constant and operations *)
  | VBool    of bool
  | VNot
  | VAnd     of bool option
  | VOr      of bool option

  (* Character constant and operations *)
  | VChar    of char

  (* String constant and operations *)
  | VString  of string

  (* Integer constant and operations *)
  | VInt     of int
  | VMod     of int option
  | VSll     of int option
  | VSrl     of int option
  | VSra     of int option

  (* Floating-point number constant and operations *)
  | VFloat   of float
  | VLog

  (* Polymorphic integer/floating-point functions *)
  | VAdd     of value option
  | VSub     of value option
  | VMul     of value option
  | VDiv     of value option
  | VNeg
  | VLt      of value option
  | VLeq     of value option
  | VGt      of value option
  | VGeq     of value option

  (* Polymorphic functions *)
  | VEq      of value option
  | VNeq     of value option

  (* Polymorphic List/String functions *)
  | VConcat  of value option

(** Returns the number of expected arguments for constants *)
let arity c = match c with

  | VClos _ | VClosIf _ | VClosMatch _ -> 1

  | VFix         -> 1

  | VRec(strs,_) -> List.length strs
  | VRecProj _   -> 1

  | VTup(i,_)    -> i
  | VTupProj _   -> 1

  | VList _      -> 1

  | VUtest(None) -> 2 | VUtest(Some _) -> 1

  | VNormal(None,None)       -> 2
  | VNormal(Some _,None)     -> 1
  | VNormal(Some _,Some _)   -> 0
  | VNormal _                -> failwith "Should not happen"
  | VUniform(None,None)      -> 2
  | VUniform(Some _, None)   -> 1
  | VUniform(Some _, Some _) -> 0
  | VUniform _               -> failwith "Should not happen"
  | VGamma(None,None)        -> 2
  | VGamma(Some _, None)     -> 1
  | VGamma(Some _, Some _)   -> 0
  | VGamma _                 -> failwith "Should not happen"
  | VExp(None)               -> 1
  | VExp _                   -> 1
  | VBern(None)              -> 1
  | VBern _                  -> 1

  | VSample -> 1

  | VLogPdf(None) -> 2 | VLogPdf(Some _) -> 1

  | VWeight -> 1

  | VResamp _ -> failwith "Resample arity should not be checked"

  | VUnit -> 0

  | VBool _    -> 0
  | VNot       -> 1
  | VAnd(None) -> 2  | VAnd(Some _) -> 1
  | VOr(None)  -> 2  | VOr(Some _)  -> 1

  | VChar _ -> 0

  | VString _ -> 0

  | VInt _     -> 0
  | VMod(None) -> 2  | VMod(Some _) -> 1
  | VSll(None) -> 2  | VSll(Some _) -> 1
  | VSrl(None) -> 2  | VSrl(Some _) -> 1
  | VSra(None) -> 2  | VSra(Some _) -> 1

  | VFloat _ -> 0
  | VLog     -> 1

  | VAdd(None) -> 2  | VAdd _ -> 1
  | VSub(None) -> 2  | VSub _ -> 1
  | VMul(None) -> 2  | VMul _ -> 1
  | VDiv(None) -> 2  | VDiv _ -> 1
  | VNeg       -> 1
  | VLt(None)  -> 2  | VLt _  -> 1
  | VLeq(None) -> 2  | VLeq _ -> 1
  | VGt(None)  -> 2  | VGt _  -> 1
  | VGeq(None) -> 2  | VGeq _ -> 1

  | VEq(None)  -> 2  | VEq(Some _)  -> 1
  | VNeq(None) -> 2  | VNeq(Some _) -> 1

  | VConcat(None) -> 2 | VConcat(Some _) -> 1

(** Returns the attribute of a term *)
let tm_attr = function
  | TVar     (a,_,_)
  | TApp     (a,_,_)
  | TLam     (a,_,_)
  | TIf      (a,_,_)
  | TMatch   (a,_)
  | TVal     (a,_) -> a

(** Returns the label of a term *)
let tm_label tm = match tm_attr tm with {label;_} -> label

(** Change the attribute of a given tm *)
let change_attr attr tm = match tm with
  | TVar   (_,x,i)   -> TVar   (attr,x,i)
  | TLam   (_,x,t1)  -> TLam   (attr,x,t1)
  | TApp   (_,t1,t2) -> TApp   (attr,t1,t2)
  | TVal   (_,c)     -> TVal   (attr,c)
  | TIf    (_,t1,t2) -> TIf    (attr,t1,t2)
  | TMatch (_,cases) -> TMatch (attr,cases)

(** Make a term stochastic *)
let set_stoch stoch tm = change_attr {(tm_attr tm) with stoch=stoch} tm

(** Check if a term is stochastic *)
let is_stoch tm = let {stoch;_} = tm_attr tm in stoch

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
let nop = TVal(na,VUnit)
let fix = TVal(na,VFix)

(** The identity function (with proper debruijn index) as a tm. *)
let idfun =
  let var,var' = genvar 0 in
  TLam(na,var,var')

(** Convenience function for creating a sequence of two tms *)
let seq t1 t2 = TApp(na,TLam(na,"_",t2),t1)

(** Function for wrapping a value in a tm *)
let tm_of_val c = TVal(na, c)

(** Used for indicating uninitialized debruijn indices *)
let noidx = -1

