(** Parser utilities *)

open Ast

(** Function for adding fix-point if a function is recursive*)
let addrec x t =
  let rec hasx t = match t with
    | TmVar(_,y,_) ->  x = y
    | TmLam(_,y,t1) -> if x = y then false else hasx t1
    | TmClos(_,_,_,_) -> failwith "Cannot happen"
    | TmApp(_,t1,t2) -> hasx t1 || hasx t2
    | TmConst _ -> false
    | TmFix _ -> false
    | TmIf(_,t,t1,t2) -> hasx t || hasx t1 || hasx t2

    | TmUtest _ -> false

    | TmMatch(_,t1,pls) -> hasx t1 || List.exists (fun (_,te) -> hasx te) pls

    | TmRec(_,rels) -> List.exists (fun (_,te) -> hasx te) rels
    | TmRecProj(_,t1,_) -> hasx t1

    | TmTup(_,tarr) -> Array.exists hasx tarr
    | TmTupProj(_,t1,_) -> hasx t1

    | TmList(_,tls) -> List.exists hasx tls

    | TmConcat _ -> false

    | TmWeight _ -> false
    | TmResamp _ -> false
  in if hasx t then
    TmApp(na,TmFix(na),TmLam(na,x,t))
  else t

(** Make a sequence of lambdas for a multi-parameter function *)
let rec mkfun params body = match params with
  | x::xs -> TmLam(na,x,mkfun xs body)
  | [] -> body

(** Make a sequence of applications for a multi-parameter function *)
let rec mkapps args func = match args with
  | t::ts -> TmApp(na,mkapps ts func,t)
  | [] -> TmVar(na,func,noidx)

