(** Module for annotating terms and variables with unique labels. *)

open Ast
open Utils
open StrMap

(** Reference used when generating new labels *)
let label = ref 0

(** Function for generating a new label *)
let next () =
  let res = !label
  in label := !label + 1; res

(** Label all variables in a term. Takes a map from strings to labels as
    argument, providing the initial environment (builtins) *)
let rec label_vars map tm = match tm with

  | TmVar(a,x,i1) ->
    (match find_opt x map with
     | Some i2 -> TmVar({a with var_label = i2},x,i1)
     | _ -> failwith ("Unbound var: " ^ x))

  | TmLam(a,x,t1) ->
      let i = next() in
      TmLam({a with var_label = i},x,label_vars (add x i map) t1)

  | _ -> tm_traverse (label_vars map) tm

(** Label all terms in a term. *)
let rec label_terms tm =

  let a = {(tm_attr tm) with label=next()} in

  let tm = match tm with
    | TmApp(_,t1,t2)      -> TmApp(a,t1,t2)
    | TmVar(_,x,i1)       -> TmVar(a,x,i1)
    | TmLam(_,x,t1)       -> TmLam(a,x,t1)
    | TmClos(_,x,t1,env)  -> TmClos(a,x,t1,env)
    | TmConst(_,c)        -> TmConst(a,c)
    | TmIf(_,t,t1,t2)     -> TmIf(a,t, t1, t2)
    | TmFix _             -> TmFix a
    | TmUtest(_,t)        -> TmUtest(a,t)
    | TmMatch(_,t1,cases) -> TmMatch(a,t1,cases)
    | TmTup(_,arr)        -> TmTup(a,arr)
    | TmTupProj(_,t1,i)   -> TmTupProj(a,t1,i)
    | TmRec(_,ls)         -> TmRec(a,ls)
    | TmRecProj(_,t1,s)   -> TmRecProj(a,t1,s)
    | TmList(_,ls)        -> TmList(a,ls)
    | TmConcat(_,t1)      -> TmConcat(a,t1)
    | TmLogPdf(_,t1)      -> TmLogPdf(a,t1)
    | TmSample _          -> TmSample a
    | TmWeight _          -> TmWeight a
    | TmResamp(_,t,c)     -> TmResamp(a,t,c)

  in tm_traverse label_terms tm

(** Given a map from strings to variable labels, a string, and a variable
    label, return true if the mapping from the string to the variable label
    exists in the map. *)
let idmatch map str id = match StrMap.find_opt str map with
  | Some i -> i = id
  | _ -> false

(** Function for labeling both variables and terms in a term. Returns the
    resulting tm, a convenient map from builtin variable names to variable
    labels, and the total number of labels (vars and regular) *)
let label builtin tm =

  (* Reset label ref *)
  label := 0;

  let builtin_map =
    List.fold_left (fun map x -> add x (next ()) map) empty builtin in

  let tm = tm |> label_vars builtin_map |> label_terms in

  tm, builtin_map, !label

