(** 0-CFA static analysis for aligning weights in programs.
    TODO Broken, needs updating *)

open Ast
open Const
open Utils
open Printf
open Debug
open Label

let align = ref false

(** Abstract values used in the 0-CFA analysis *)
type absval =
  | Stoch
  | Fun of { louter:int; linner:int; lvar:int }
  | Fix

(** Constraints used in the 0-CFA analysis *)
type cstr =
  | Dir of absval * int
  | Sub of int * int
  | Impl of absval * int * int * int

(** Returns string representation of abstract values *)
let string_of_absval = function
  | Stoch -> "Stoch"
  | Fun{louter;linner;lvar} ->
    "Fun(" ^
    (String.concat "," (List.map string_of_int [louter;linner;lvar])) ^ ")"
  | Fix -> "Fix"

(** Returns string representation of constraints *)
let string_of_cstr = function
  | Dir(av,n) -> string_of_absval av ^ " in " ^ string_of_int n
  | Sub(n1,n2) -> string_of_int n1 ^ " in " ^ string_of_int n2
  | Impl(av,n1,n2,n3) ->
    string_of_absval av ^ " in " ^ string_of_int n1
    ^ " => " ^ string_of_int n2 ^ " in " ^ string_of_int n3

(** Returns abstract value representations of all functions in a program *)
let functions tm =
  let rec recurse tm funs = match tm with
    | TmVar _ -> funs
    | TmLam({label;var_label;_},_,t1) ->
      Fun{louter=label;linner=tm_label t1;lvar=var_label} :: recurse t1 funs
    | TmApp(_,t1,t2) -> funs |> recurse t1 |> recurse t2
    | TmConst _ | TmIf _ | TmFix _ (* TODO if *)
    | TmClos _ -> failwith "Closure before eval"
    | TmUtest _ -> failwith "TODO analysis.ml"

    | TmMatch _ -> failwith "TODO analysis.ml"
    | TmTup _ -> failwith "TODO analysis.ml"
    | TmTupProj _ -> failwith "TODO analysis.ml"

    | TmRec _ | TmRecProj _ -> failwith "TODO analysis.ml"

    | TmList _ -> failwith "TODO analysis.ml"
    | TmConcat _ -> failwith "TODO analysis.ml"

    | TmLogPdf _ -> failwith "TODO analysis.ml"
    | TmSample _ -> failwith "TODO analysis.ml"
    | TmWeight _ -> failwith "TODO analysis.ml"
    | TmResamp _ -> failwith "TODO analysis.ml"

  in recurse tm []

(** Generate a set of 0-CFA constraints for a program. For now, built in
    functions must be applied immediately where occuring (no currying). *)
let gen_cstrs bmap tm =
  let funs = functions tm in
  let rec recurse tm cstrs = match tm with

    (* Binary operators *)
    | TmApp({label=l;_},TmApp(_,TmConst(_,const),t1),t2)
      when arity const = 2 ->
      let l1 = tm_label t1 in
      let l2 = tm_label t2 in
      let cstrs = cstrs |> recurse t1 |> recurse t2 in
      Sub(l1,l) :: Sub(l2,l) :: cstrs

    (* Unary operators *)
    | TmApp({label=l;_},TmConst(_,const),t1)
      when arity const = 1 ->
      let l1 = tm_label t1 in
      let cstrs = cstrs |> recurse t1 in
      Sub(l1,l) :: cstrs

    (* If expressions *)
    | TmIf({label=l;_}, t1, t2, t3) ->
      let l2 = tm_label t2 in
      let l3 = tm_label t3 in
      let cstrs = cstrs |> recurse t1 |> recurse t2 |> recurse t3 in
      Sub(l2,l) :: Sub(l3,l) :: cstrs

    (* Sample *)
    | TmApp({label=l;_},TmVar({var_label;_},_,_),t1)
      when idmatch bmap "sample" var_label ->
      let cstrs = cstrs |> recurse t1 in
      Dir(Stoch, l) :: cstrs

    (* Fixpoint *)
    | TmApp({label;_},TmFix(_), t1) ->
      let l = label in
      let l1 = tm_label t1 in
      let cstrs = cstrs |> recurse t1 in
      List.fold_left
        (fun cstrs av -> match av with
           | Fun{linner=l2;lvar=x;_} ->
             Impl(av,l1,l2,x) :: Impl(av,l1,l2,l) :: cstrs
           | _ -> failwith "Non-fun absval in funs")
        cstrs funs

    (* Variables *)
    | TmVar({label=l;var_label=x;_},_,_) -> Sub(x, l) :: cstrs

    (* Lambdas *)
    | TmLam({label;var_label;_},_,t1) ->
      Dir(Fun{louter=label;linner=tm_label t1;lvar=var_label},label)
      :: recurse t1 cstrs

    (* General applications (not caught by operators/keywords above) *)
    | TmApp({label=l;_},t1,t2) ->
      let l1 = tm_label t1 in
      let l2 = tm_label t2 in
      let cstrs = cstrs |> recurse t1 |> recurse t2 in
      List.fold_left
        (fun cstrs av -> match av with
           | Fun{linner=l3;lvar=x;_} ->
             Impl(av,l1,l2,x) :: Impl(av,l1,l3,l) :: cstrs
           | _ -> failwith "Non-fun absval in funs")
        cstrs funs

    | TmConst _ -> cstrs

    | TmRec _ | TmRecProj _ -> failwith "TODO analysis.ml"

    | TmMatch _ -> failwith "TODO analysis.ml"
    | TmTup _ -> failwith "TODO analysis.ml"
    | TmTupProj _ -> failwith "TODO analysis.ml"

    | TmClos _ -> failwith "Closure before eval"
    | TmUtest _ -> failwith "TODO analysis.ml"

    | TmFix _ -> failwith "TODO analysis.ml"

    | TmList _ -> failwith "TODO analysis.ml"
    | TmConcat _ -> failwith "TODO analysis.ml"

    | TmLogPdf _ -> failwith "TODO analysis.ml"
    | TmSample _ -> failwith "TODO analysis.ml"
    | TmWeight _ -> failwith "TODO analysis.ml"
    | TmResamp _ -> failwith "TODO analysis.ml"

  in recurse tm []

(** Sets of abstract values *)
module AbsValSet = Set.Make(struct let compare = compare type t = absval end)

(** Analyze the program using 0-CFA to discover dynamic parts *)
let analyze bmap tm nl =
  let open AbsValSet in
  let worklist = ref [] in
  let data = Array.make nl empty in
  let edges = Array.make nl [] in
  let cstrs = gen_cstrs bmap tm in
  let add q d =
    if not (subset d data.(q)) then
      (data.(q) <- union data.(q) d;
       worklist := q :: !worklist) in

  debug debug_sanalysis "Constraints"
    (fun () ->
       String.concat "\n" (List.map (fun cstr -> string_of_cstr cstr) cstrs));

  (* Building the graph *)
  let f cstr = match cstr with
    | Dir(t,p) -> add p (singleton t)
    | Sub(p1,_) -> edges.(p1) <- cstr :: edges.(p1)
    | Impl(_,p,p1,_) ->
      edges.(p1) <- cstr :: edges.(p1);
      edges.(p)  <- cstr :: edges.(p) in
  List.iter f cstrs;

  (* Iteration *)
  while match !worklist with [] -> false | _ -> true do
    let q = List.hd !worklist in
    worklist := List.tl !worklist;
    let f cstr = match cstr with
      | Sub(p1,p2) -> add p2 data.(p1)
      | Impl(t,p,p1,p2) -> if mem t data.(p) then add p2 data.(p1)
      | Dir _ -> failwith "Direct constraint in iteration" in
    List.iter f edges.(q)
  done;

  (* Mark dynamic parts *)
  let mark = Array.make nl false in
  let modified = ref true in

  let rec recurse flag tm =
    let l = tm_label tm in
    if flag || mark.(l) then
      (if not mark.(l) then (mark.(l) <- true; modified := true);
       iter (fun av -> match av with
           | Fun{louter=l;_} ->
             if not mark.(l) then (mark.(l) <- true; modified := true)
           | _ -> ())
         data.(l));
    match tm with
    | TmIf(_,t1,t2,t3)
      when not flag ->
      recurse flag t1;
      let flag = mem Stoch data.(tm_label t1) in
      recurse flag t2; recurse flag t3

    | TmVar _ -> ()
    | TmLam({label=l;_},_,t1) -> recurse (mark.(l) || flag) t1

    | TmApp(_,t1,t2) -> recurse flag t1; recurse flag t2;


    | TmConst _ | TmIf _ | TmFix _ -> () (* TODO if *)

    | TmMatch _ -> failwith "TODO analysis.ml"
    | TmTup _ -> failwith "TODO analysis.ml"
    | TmTupProj _ -> failwith "TODO analysis.ml"

    | TmRec _ -> failwith "TODO analysis.ml"
    | TmRecProj _ -> failwith "TODO analysis.ml"
    | TmClos _ -> failwith "Closure before eval"
    | TmUtest _ -> failwith "TODO analysis.ml"

    | TmList _ -> failwith "TODO analysis.ml"
    | TmConcat _ -> failwith "TODO analysis.ml"

    | TmLogPdf _ -> failwith "TODO analysis.ml"
    | TmSample _ -> failwith "TODO analysis.ml"
    | TmWeight _ -> failwith "TODO analysis.ml"
    | TmResamp _ -> failwith "TODO analysis.ml"

  in while !modified do
    modified := false;
    recurse false tm;
  done;

  debug debug_sanalysis "Data"
    (fun () -> String.concat "\n"
     (List.mapi (fun i set ->
           sprintf "Label %d: { %s }" i
             (String.concat ", "
                (List.map string_of_absval (elements set))))
        (Array.to_list data)));

  debug debug_sanalysis "Dynamic"
    (fun () -> String.concat "\n"
       (List.mapi
          (fun i b ->
             sprintf "Label %d = %B" i b) (Array.to_list mark)));

  mark

(** Transform all dynamic weights to dweights. We ignore other synchronization
    checkpoints for now since we are only dealing with SMC. *)
let align_weight bmap dyn tm =
  let idmatch str id =
    match StrMap.find_opt str bmap with
    | Some i -> i = id
    | _ -> false in
  let rec recurse tm = match tm with

    (* Replace static weight with a weight followed by resample.
       TODO Cleanup *)
    | TmVar({label;var_label;_},_,_)
      when idmatch "weight" var_label ->
      if not dyn.(label) then
        let var_name = "f" in
        let var = TmVar(na,var_name,noidx) in
        TmLam(na,var_name,
              TmApp(na,
                    TmLam(na,"_",TmResamp(na,None,None)),
                    TmApp(na,tm,var)))

      else tm

    | TmLam(a,x,t1) -> TmLam(a,x,recurse t1)

    | TmApp(a,t1,t2) -> TmApp(a,recurse t1,recurse t2)

    | TmFix _ | TmVar _ | TmConst _
    | TmIf _ -> tm (* TODO if *)

    | TmMatch _ -> failwith "TODO analysis.ml"
    | TmTup _ -> failwith "TODO analysis.ml"
    | TmTupProj _ -> failwith "TODO analysis.ml"

    | TmRec _ | TmRecProj _ -> failwith "TODO analysis.ml"

    | TmClos _ -> failwith "Closure before eval"
    | TmUtest _ -> failwith "TODO analysis.ml"

    | TmList _ -> failwith "TODO analysis.ml"
    | TmConcat _ -> failwith "TODO analysis.ml"

    | TmLogPdf _ -> failwith "TODO analysis.ml"
    | TmSample _ -> failwith "TODO analysis.ml"
    | TmWeight _ -> failwith "TODO analysis.ml"
    | TmResamp _ -> failwith "TODO analysis.ml"

  in recurse tm

