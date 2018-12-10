(** Definitions and operations on the pplcore abstract syntax tree and
    environment *)

open Const
open Pattern
open Printf

(** Attributes of terms.
   Can easily be extended with more data fields as needed. *)
type attr = { label:int; var_label:int; pos:Lexing.position }

(** Default attribute with dummy values *)
let na = { label = -1; var_label = -1; pos = Lexing.dummy_pos }

(** Core terms/expressions *)
type tm =

  (* Lambda Calculus *)
  | TmVar         of attr * string * int
  | TmLam         of attr * string * tm
  | TmClos        of attr * string * tm * env
  | TmApp         of attr * tm * tm

  (* Constants *)
  | TmConst       of attr * const

  (* If expressions using the bool constants to dictate control flow *)
  | TmIf          of attr * bool option * tm option

  (* Fixed-point combinator (not really needed since untyped) *)
  | TmFix         of attr

  (* Pattern matching construct *)
  | TmMatch       of attr * tm * (pat * tm) list

  (* Records and record projection. Use linear search for projection
     TODO Use hashtable for larger records? *)
  | TmRec         of attr * (string * tm) list
  | TmRecProj     of attr * tm * string

  (* Tuples and tuple projection. Uses O(1) array lookup for projection *)
  | TmTup         of attr * tm array
  | TmTupProj     of attr * tm * int

  (* Lists *)
  | TmList        of attr * tm list

  (* Polymorphic concatenation function (Lists and Strings for now) *)
  | TmConcat      of attr * tm option

  (* Probabilistic programming functions *)
  | TmInfer       of attr
  | TmLogPdf      of attr * tm option
  | TmSample      of attr * tm option * tm option
  | TmWeight      of attr * tm option * const option
  | TmDWeight     of attr * tm option * const option

  (* Construct for performing unit tests TODO move to toplevel? *)
  | TmUtest       of attr * tm * tm

(** Evaluation environment *)
and env = tm list

(** Check if two value terms are equal. Does not check for equality of lambdas,
    which in general should be undecidable *)
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

(** Returns the label of a term *)
let tm_label = function
  | TmVar({label;_},_,_)
  | TmLam({label;_},_,_)
  | TmClos({label;_},_,_,_)
  | TmApp({label;_},_,_)
  | TmConst({label;_},_)
  | TmIf({label;_},_,_)
  | TmFix({label;_})
  | TmUtest({label;_},_,_)
  | TmMatch({label;_},_,_)
  | TmRec({label;_},_)
  | TmRecProj({label;_},_,_)
  | TmTup({label;_},_)
  | TmTupProj({label;_},_,_)
  | TmList({label;_},_)
  | TmConcat({label;_},_)
  | TmInfer{label;_}
  | TmLogPdf({label;_},_)
  | TmSample({label;_},_,_)
  | TmWeight({label;_},_,_)
  | TmDWeight({label;_},_,_) -> label

(** Convert terms to strings TODO Clean this up *)
let string_of_tm t =

  let rec rec1 prec t =
    let p = rec2 t in
    let paren = match t with
      | TmMatch _ | TmLam _ | TmClos _ -> prec > 0
      | TmApp _  -> prec > 1
      | TmTup _ | TmVar _ | TmConst _ | TmFix _
      | TmIf _ | TmRec _ | TmTupProj _ | TmRecProj _ | TmList _
      | TmInfer _ | TmLogPdf _ | TmSample _ | TmUtest _
      | TmWeight _ | TmDWeight _ | TmConcat _ -> false
    in if paren then "(" ^ p ^ ")" else p

  and rec2 t =
    match t with
    | TmVar(_,x,n) -> x ^ "#" ^ string_of_int n
    | TmLam(_,x,t1) -> "lam " ^ x ^ ". " ^ rec1 0 t1
    | TmClos(_,x,t,_) -> "clos " ^ x ^ ". " ^ rec1 0 t
    | TmApp(_,t1,(TmApp _ as t2)) -> rec1 1 t1 ^ " " ^ rec1 2 t2
    | TmApp(_,t1,t2) -> rec1 1 t1 ^ " " ^ rec1 1 t2
    | TmConst(_,c) -> string_of_const c
    | TmIf(_,None,_) -> "if"
    | TmIf(_,Some(g),Some(t2)) ->
      "if(" ^ string_of_bool g ^ "," ^ rec1 0 t2 ^ ")"
    | TmIf(_,Some(g),_) -> "if(" ^ string_of_bool g ^ ")"
    | TmFix _ -> "fix"
    | TmUtest(_,t1,t2) -> "utest(" ^ rec1 0 t1 ^ "," ^ rec1 0 t2 ^ ")"

    | TmMatch(_,t,cases) ->
      let inner = List.map (fun (_,t1) -> "| p -> " ^ rec1 0 t1) cases in
      "match " ^ rec1 0 t ^ " with " ^ (String.concat " " inner)

    | TmRec(_,sm) ->
      let inner = List.map (fun (k, t1) -> k ^ ":" ^ rec1 0 t1) sm in
      "{" ^ (String.concat (",") inner) ^ "}"

    | TmRecProj(_,t1,x) -> rec1 2 t1 ^ "." ^ x

    | TmTup(_,tarr) ->
      let inner = Array.map (fun t1 -> rec1 0 t1) tarr in
      "(" ^ (String.concat (",") (Array.to_list inner)) ^ ")"

    | TmTupProj(_,t1,i) -> rec1 2 t1 ^ "." ^ (string_of_int i)

    | TmList(_,ls) ->
      let inner = List.map (fun t1 -> rec1 0 t1) ls in
      "[" ^ (String.concat (";") inner) ^ "]"

    | TmConcat(_,None) -> "concat"
    | TmConcat(_,Some t1) -> sprintf "concat(%s)" (rec1 0 t1)

    | TmInfer _ -> "infer"
    | TmLogPdf(_,None) -> "logpdf"
    | TmLogPdf(_,Some t1) -> sprintf "logpdf(%s)" (rec1 0 t1)
    | TmSample(_,None,None) -> "sample"
    | TmSample(_,Some t1,None) -> sprintf "sample(%s)" (rec1 0 t1)
    | TmSample(_,Some t1,Some t2) ->
      sprintf "sample(%s,%s)" (rec1 0 t1) (rec1 0 t2)
    | TmSample _ -> failwith "Incorrect sample in string_of_tm"
    | TmWeight(_,None,None) -> "weight"
    | TmWeight(_,Some t1,None) -> sprintf "weight(%s)" (rec1 0 t1)
    | TmWeight(_,Some t1,Some c2) ->
      sprintf "weight(%s,%s)" (rec1 0 t1) (string_of_const c2)
    | TmWeight _ -> failwith "Incorrect weight in string_of_tm"
    | TmDWeight(_,None,None) -> "dweight"
    | TmDWeight(_,Some t1,None) -> sprintf "dweight(%s)" (rec1 0 t1)
    | TmDWeight(_,Some t1,Some c2) ->
      sprintf "dweight(%s,%s)" (rec1 0 t1) (string_of_const c2)
    | TmDWeight _ -> failwith "Incorrect dweight in string_of_tm"

  in rec1 (-1) t

(** Convert terms to string with labels included. TODO Reuse code from the above *)
let rec lstring_of_tm = function

  | TmVar({label;var_label;_},x,_) ->
    x ^ "|" ^ (string_of_int var_label) ^ ":" ^ (string_of_int label)

  | TmLam({label;var_label;_},x,t1) ->
    "(lam " ^ x ^ "|" ^ (string_of_int var_label) ^ ". " ^
    (lstring_of_tm t1) ^ "):" ^ (string_of_int label)

  | TmClos _ -> "Closure"

  | TmApp({label;_},t1,t2) -> "(" ^ lstring_of_tm t1 ^ " " ^ lstring_of_tm t2
                                ^ "):" ^ (string_of_int label)
  | TmConst({label;_},c) -> string_of_const c ^ ":" ^ (string_of_int label)
  | TmIf({label;_},_,_) -> "if:" ^ (string_of_int label)
  | TmFix({label;_}) -> "fix:" ^ (string_of_int label)

  | TmUtest _ -> failwith "TODO"

  | TmMatch _ -> failwith "TODO"

  (* Records *)
  | TmRec({label;_},sm) ->
      let inner = List.map (fun (k, t1) -> k ^ ":" ^ lstring_of_tm t1) sm in
      "{ " ^ (String.concat (",") inner) ^ " }:"
      ^ (string_of_int label)
  | TmRecProj({label;_},t1,x) -> lstring_of_tm t1 ^ "." ^ x ^ ":" ^
                              (string_of_int label)

  | TmTup _ -> failwith "TODO"

  | TmTupProj _ -> failwith "TODO"

  | TmList _ -> failwith "TODO"
  | TmConcat _ -> failwith "TODO"

  | TmInfer _ -> failwith "TODO"
  | TmLogPdf _ -> failwith "TODO"
  | TmSample _ -> failwith "TODO"
  | TmWeight _ -> failwith "TODO"
  | TmDWeight _ -> failwith "TODO"

(** Convert environments to strings *)
let string_of_env env =
  "[" ^ (List.mapi (fun i t -> sprintf " %d -> " i ^ string_of_tm t) env
            |> String.concat ",") ^ "]"

(** Unit shortcut *)
let nop = TmConst(na, CUnit)

(** Function for wrapping a const in a tm *)
let tm_of_const c = TmConst(na, c)

(** Used for indicating uninitialized debruijn indices *)
let noidx = -1

(** Reference used for genvar *)
let nextvar = ref 0

(** Generate fresh variable names for CPS transformation.  Avoids clashes by
    using $ as first char (not allowed in lexer for vars).  Takes a debruijn
    index as argument (for idfun). *)
let genvar i =
  let res = !nextvar in
  let str = "$" ^ string_of_int res in
  nextvar := res + 1;
  (str, TmVar(na,str,i))

(** The identity function (with proper debruijn index) as a tm. *)
let idfun =
  let var, var' = genvar 0 in
  TmLam(na,var,var')


