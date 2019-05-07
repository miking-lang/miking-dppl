(** Definitions and operations on the pplcore abstract syntax tree and
    environment. *)

open Const
open Pattern

(** Attributes of terms.
   Can easily be extended with more data fields as needed. *)
type attr = { label:int; var_label:int; pos:Lexing.position }

(** Dummy value for labels *)
let no_label = -1

(** Default attribute with dummy values *)
let na = { label = no_label; var_label = no_label; pos = Lexing.dummy_pos }

(** Core terms/expressions *)
type tm =

  (* Lambda Calculus *)
  | TmVar     of attr * string * int
  | TmLam     of attr * string * tm
  | TmClos    of attr * string * tm * env
  | TmApp     of attr * tm * tm

  (* Constants *)
  | TmConst   of attr * const

  (* Pattern matching construct *)
  | TmMatch   of attr * tm * (pat * tm) list

  (* If expressions using the bool constants (could be encoded as TmMatch) *)
  | TmIf      of attr * tm * tm * tm

  (* Fixed-point combinator (not really needed since untyped) *)
  | TmFix     of attr

  (* Records and record projection. Use linear search for projection.
     Should we use hashtable for larger records? *)
  | TmRec     of attr * (string * tm) list
  | TmRecProj of attr * tm * string

  (* Tuples and tuple projection. Uses O(1) array lookup for projection *)
  | TmTup     of attr * tm array
  | TmTupProj of attr * tm * int

  (* Lists TODO Add functions for building lists *)
  | TmList    of attr * tm list

  (* Polymorphic concatenation function (Lists and Strings for now) *)
  | TmConcat  of attr * tm option

  (* Construct for performing unit tests *)
  | TmUtest   of attr * tm option

  (* Distribution functions (We could move these to separate data type,
     analogous to const.ml) *)
  | TmLogPdf  of attr * tm option
  | TmSample  of attr

  (* Weighting of executions *)
  | TmWeight  of attr

  (* Resample checkpoint for SMC inference (natively in CPS form) *)
  | TmResamp  of attr * tm option * const option

(** Evaluation environment *)
and env = tm list

(** Check if two value terms are equal.
    Does not check for equality of lambdas. *)
let rec val_equal v1 v2 = match v1,v2 with

  | TmRec(_,rels1),TmRec(_,rels2) ->
    let comp (k1,v1) (k2,v2) = k1 = k2 && val_equal v1 v2 in
    (try List.for_all2 comp rels1 rels2 with Invalid_argument _ -> false)

  | TmList(_,ls1),TmList(_,ls2) ->
    (try List.for_all2 val_equal ls1 ls2 with Invalid_argument _ -> false)

  | TmTup(_,tarr1),TmTup(_,tarr2) ->
    let ls1 = Array.to_list tarr1 in
    let ls2 = Array.to_list tarr2 in
    (try List.for_all2 val_equal ls1 ls2 with Invalid_argument _ -> false)

  | TmConst(_,c1),TmConst(_,c2) -> c1 = c2

  | _ -> false

(* Convenience function for recursively traversing the abstract syntax tree of
   a program. The argument f is a function that handles a tm in some way, or
   simply calls traverse to reach other parts of the tree *)
let tm_traverse f tm = match tm with

  | TmLam(a,x,t1)     -> TmLam(a,x,f t1)
  | TmIf(a,t,t1,t2)   -> TmIf(a,f t,f t1,f t2)
  | TmApp(a,t1,t2)    -> TmApp(a,f t1,f t2)
  | TmTup(a,arr)      -> TmTup(a,Array.map f arr)
  | TmTupProj(a,t1,i) -> TmTupProj(a,f t1,i)
  | TmRec(a,sm)       -> TmRec(a,List.map (fun (k,v) -> k,f v) sm)
  | TmRecProj(a,t1,x) -> TmRecProj(a,f t1,x)
  | TmList(a,ls)      -> TmList(a,List.map f ls)
  | TmMatch(a,tm,cases)
    -> TmMatch(a,f tm, List.map (fun (p,tm) -> (p, f tm)) cases)

  | TmClos _             -> failwith "ERROR: Traversing a closure\n"
  | TmConcat(_,Some _)   -> failwith "ERROR: Traversing a concat\n"
  | TmLogPdf(_,Some _)   -> failwith "ERROR: Traversing a logpdf\n"
  | TmResamp(_,Some _,_) -> failwith "ERROR: Traversing a resample\n"
  | TmUtest(_,Some _)    -> failwith "ERROR: Traversing a utest\n"

  | TmConcat _ | TmLogPdf _ | TmResamp _
  | TmWeight _ | TmVar _ | TmFix _
  | TmSample _ | TmConst _ | TmUtest _ -> tm

(** Returns the attribute of a term *)
let tm_attr = function
  | TmVar     (a,_,_)
  | TmLam     (a,_,_)
  | TmClos    (a,_,_,_)
  | TmApp     (a,_,_)
  | TmConst   (a,_)
  | TmIf      (a,_,_,_)
  | TmFix     (a)
  | TmUtest   (a,_)
  | TmMatch   (a,_,_)
  | TmRec     (a,_)
  | TmRecProj (a,_,_)
  | TmTup     (a,_)
  | TmTupProj (a,_,_)
  | TmList    (a,_)
  | TmConcat  (a,_)
  | TmLogPdf  (a,_)
  | TmSample  (a)
  | TmWeight  (a)
  | TmResamp  (a,_,_) -> a

(** Returns the label of a term *)
let tm_label tm = match tm_attr tm with {label;_} -> label

(** Reference used for genvar *)
let nextvar = ref 0

(** Generate fresh variable names (used for CPS transformation).  Avoids
    clashes by using $ as first char (not allowed in lexer for vars).  Takes a
    debruijn index as argument (for idfun). *)
let genvar i =
  let res = !nextvar in
  let str = "$" ^ string_of_int res in
  nextvar := res + 1;
  (str, TmVar(na,str,i))

(** Similar to genvar, but specify your own variable name *)
let makevar str i = (str, TmVar(na,str,i))

(** Unit shorthand *)
let nop = TmConst(na, CUnit)

(** The identity function (with proper debruijn index) as a tm. *)
let idfun =
  let var, var' = genvar 0 in
  TmLam(na,var,var')

(** Convenience function for creating a sequence of two tms *)
let seq t1 t2 = TmApp(na,TmLam(na,"_",t2),t1)

(** Function for wrapping a const in a tm *)
let tm_of_const c = TmConst(na, c)

(** Used for indicating uninitialized debruijn indices *)
let noidx = -1

