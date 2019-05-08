(** Definitions and operations on the pplcore abstract syntax tree and
    environment. *)

open Const
open Pattern
open Format

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
  | TmResamp(_,Some _,_) -> failwith "ERROR: Traversing a resample\n"
  | TmUtest(_,Some _)    -> failwith "ERROR: Traversing a utest\n"

  | TmConcat _ | TmResamp _
  | TmWeight _ | TmVar _    | TmFix _
  | TmConst _  | TmUtest _ -> tm

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

(** Precedence constants for printing *)
type prec =
  | MATCH
  | LAM
  | SEMICOLON
  | IF
  | TUP
  | APP
  | ATOM

(** Simple enum used in the concat function in string_of_tm *)
type sep =
  | SPACE
  | COMMA

(** Convert terms to strings. *)
let string_of_tm
    ?(debruijn   = false)
    ?(labels     = false)
    ?(pretty     = true)
    ?(indent     = 2)
    ?(max_indent = 68)
    ?(margin     = 80)
    t =

  (* Set right margin and maximum indentation *)
  pp_set_margin str_formatter margin;
  pp_set_max_indent str_formatter max_indent;

  (* Function for concatenating a list of fprintf calls using a given
     separator. *)
  let rec concat fmt (sep, ls) = match ls with
    | [] -> ()
    | [f] -> f fmt
    | f :: ls -> match sep with
      | SPACE -> fprintf fmt "%t@ %a" f concat (sep, ls)
      | COMMA -> fprintf fmt "%t,@,%a" f concat (sep, ls)
  in

  let rec recurse fmt (prec, t) =

    (* Function for bare printing (no syntactic sugar) *)
    let bare fmt t = match t with

      | TmMatch(_,t,cases) ->
        let inner = List.map (fun (p,t1) ->
            (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" indent
                (string_of_pat p) recurse (LAM, t1)))
            cases in
        fprintf fmt "@[<hov %d>match@ %a@ with@ @[<hv 0>%a@]@]"
          indent
          recurse (MATCH, t)
          concat (SPACE,inner)

      | TmLam(_,x,t1) ->
        fprintf fmt "@[<hov %d>lam %s.@ %a@]" indent x recurse (MATCH, t1)

      (* TODO Also print closure environment if an argument is given *)
      | TmClos(_,x,t1,_) ->
        fprintf fmt "@[<hov %d>clos %s.@ %a@]" indent x recurse (MATCH, t1)

      | TmIf(_,t1,t11,t12) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>if %a then@ %a@]\
                     @ \
                     @[<hov %d>else@ %a@]\
                     @]"
          indent recurse (MATCH, t1) recurse (MATCH, t11)
          indent recurse (MATCH, t12)

      | TmTup(_,tarr) ->
        let inner = Array.map (fun t1 ->
            (fun fmt -> fprintf fmt "%a" recurse (APP, t1))) tarr in
        fprintf fmt "@[<hov 0>%a@]"
          concat (COMMA,Array.to_list inner)

      | TmRec(_,sm) ->
        let inner = List.map (fun (k, t1) ->
           (fun fmt -> fprintf fmt "%s:%a" k recurse (MATCH, t1))) sm in
        fprintf fmt "{@[<hov 0>%a@]}"
          concat (COMMA,inner)

      | TmList(_,ls) ->
        let inner = List.map (fun t1 ->
           (fun fmt -> fprintf fmt "%a" recurse (MATCH, t1))) ls in
        fprintf fmt "[@[<hov 0>%a@]]"
          concat (COMMA,inner)

      (* TODO Make applications look nicer *)
      | TmApp(_,t1,(TmApp _ as t2)) ->
        fprintf fmt "@[<hv 0>%a@ %a@]" recurse (APP, t1) recurse (ATOM, t2)

      | TmApp(_,t1,t2) ->
        fprintf fmt "@[<hv 0>%a@ %a@]" recurse (APP, t1) recurse (APP, t2)

      | TmVar({var_label;_},x,n) ->
        let vl = if debruijn then Printf.sprintf "#%d" n else "" in
        let d = if labels then Printf.sprintf "|%d" var_label else "" in
        if labels || debruijn then
          fprintf fmt "<%s%s%s>" x vl d
        else fprintf fmt "%s" x

      | TmRecProj(_,t1,x)   -> fprintf fmt "%a.%s" recurse (APP, t1) x
      | TmTupProj(_,t1,i)   -> fprintf fmt "%a.%d" recurse (APP, t1) i

      | TmConst(_,c)        -> fprintf fmt "%s" (string_of_const c)

      | TmFix _             -> fprintf fmt "fix"

      | TmUtest(_,Some t1)  -> fprintf fmt "utest(%a)" recurse (MATCH, t1)
      | TmUtest _           -> fprintf fmt "utest"

      | TmConcat(_,None)    -> fprintf fmt "concat"
      | TmConcat(_,Some t1) -> fprintf fmt "concat(%a)" recurse (MATCH, t1)

      | TmWeight _          -> fprintf fmt "weight"

      | TmResamp(_,None,None)       -> fprintf fmt "resample"
      | TmResamp(_,Some t1,None)    -> fprintf fmt "resample(%a)"
                                         recurse (MATCH, t1)
      | TmResamp(_,Some t1,Some c1) -> fprintf fmt "resample(%a,%s)"
                                         recurse (APP, t1)
                                         (string_of_const c1)
      | TmResamp _                  -> failwith "Invalid TmResamp"

    in

    (* Syntactic sugar printing *)
    let sugar fmt t = match t with

      (* Sequencing (right associative) *)
      | TmApp(_,TmLam(_,"_",t2),
              (TmApp(_,TmLam(_,"_",_),_) as t1)) ->
        fprintf fmt "@[<hv 0>%a;@ %a@]"
          recurse (IF, t1) recurse (MATCH, t2)
      | TmApp(_,TmLam(_,"_",t2),t1) ->
        fprintf fmt "@[<hv 0>%a;@ %a@]"
          recurse (SEMICOLON, t1) recurse (MATCH, t2)

      (* Let expressions *)
      | TmApp(_,TmLam(_,x,t1),t2) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>let %s =@ %a in@]\
                     @ %a@]"
          indent x recurse (MATCH, t2) recurse (MATCH, t1)

      | _ -> bare fmt t
    in

    (* Check if this term should be parenthesized *)
    let paren = match t with

      | TmApp(_,TmLam(_,"_",_),_) when pretty -> prec > SEMICOLON
      | TmApp(_,TmLam(_,_,_),_) when pretty   -> prec > LAM

      | TmMatch _          -> prec > MATCH
      | TmLam _ | TmClos _ -> prec > LAM
      | TmIf _             -> prec > IF
      | TmTup _            -> prec > TUP
      | TmApp _            -> prec > APP
      | TmVar _     | TmConst _
      | TmFix _     | TmRec _     | TmTupProj _
      | TmRecProj _ | TmList _ | TmUtest _   | TmWeight _
      | TmConcat _  | TmResamp _ -> prec > ATOM

    in

    let p = if pretty then sugar else bare in

    if labels then
      match t with
      | TmResamp _ | TmVar _  | TmConst _
      | TmWeight _ | TmFix _  | TmRec _
      | TmTupProj _ | TmRecProj _ | TmList _ | TmUtest _
      | TmConcat _ -> fprintf fmt "%a:%d"   p t (tm_label t)
      | _          -> fprintf fmt "(%a):%d" p t (tm_label t)
    else if paren then
      fprintf fmt "(%a)" p t
    else
      fprintf fmt "%a" p t

  in recurse str_formatter (MATCH, t); flush_str_formatter ()

(** Convert environments to strings TODO Merge with TmClos in string_of_tm *)
let string_of_env env =
  "[" ^ (List.mapi (fun i t -> Printf.sprintf " %d -> " i ^ string_of_tm t) env
            |> String.concat ",") ^ "]"

