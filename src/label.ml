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

  | TVar(a,x,i1) ->
    (match find_opt x map with
     | Some i2 -> TVar({a with var_label = i2},x,i1)
     | _ -> failwith ("Unbound var: " ^ x))

  | TApp(a,t1,t2) -> TApp(a,label_vars map t1,label_vars map t2)

  | TLam(a,x,t1) ->
      let i = next() in
      TLam({a with var_label = i},x,label_vars (add x i map) t1)

  | TIf(a,t1,t2) -> TIf(a,label_vars map t1,label_vars map t2)

  (* TODO Also label variables bound by TmMatch *)
  | TMatch(a,cls) -> TMatch(a,List.map (fun (p,t) -> p,label_vars map t) cls)

  | TVal _ -> tm

(** Label all terms in a term. *)
let rec label_terms tm =

  let a = {(tm_attr tm) with label=next()} in

  match tm with
    | TVar(_,x,i1)  -> TVar(a,x,i1)
    | TApp(_,t1,t2) -> TApp(a,label_terms t1,label_terms t2)
    | TLam(_,x,t1)  -> TLam(a,x,label_terms t1)
    | TIf(_,t1,t2)  -> TIf(a,label_terms t1,label_terms t2)
    | TMatch(_,cls) -> TMatch(a,List.map (fun (p,t) -> p,label_terms t) cls)
    | TVal(c)       -> TVal(val_update_attr a c)

(** Function for labeling both variables and terms in a term. Returns the
    resulting tm, a convenient map from builtin variable names to variable
    labels, and the total number of labels (vars and regular) *)
let label builtin tm =

  (* Reset label ref *)
  label := 0;

  (* Begin by labeling terms in builtins *)
  let builtin = builtin
                |> List.map (fun (x,t) ->
                    x, t |> label_vars empty |> label_terms) in

  (* The term tm has builtin as environment. Initialize the map required
     by label_vars from builtins *)
  let builtin_map =
    List.fold_left (fun map x -> add x (next ()) map)
      empty (builtin |> List.split |> fst) in

  (* Finally label the actual term *)
  let tm = tm |> label_vars builtin_map |> label_terms in

  tm, builtin, !label

