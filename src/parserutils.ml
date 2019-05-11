(** Parser utilities *)

open Ast

(** Function for adding fix-point if a function is recursive*)
let addrec x t =

  let rec hasx t = match t with
    | TVar(_,y,_) ->  x = y
    | TApp(_,t1,t2) -> hasx t1 || hasx t2
    | TLam(_,y,t1) -> if x = y then false else hasx t1
    | TIf(_,t1,t2) -> hasx t1 || hasx t2
    | TMatch(_,pls) -> List.exists (fun (_,te) -> hasx te) pls

    | TVal _ -> false (* TODO *) in

  if hasx t then
    TApp(na,fix,TLam(na,x,t))
  else t

(** Make a sequence of lambdas for a multi-parameter function *)
let rec mkfun params body = match params with
  | x::xs -> TLam(na,x,mkfun xs body)
  | [] -> body

(** Make a sequence of applications for a multi-parameter function *)
let rec mkapps args func = match args with
  | t::ts -> TApp(na,mkapps ts func,t)
  | [] -> TVar(na,func,noidx)

