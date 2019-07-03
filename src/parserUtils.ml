(** Parser utilities *)

open Ast

(** Function for adding fix-point if a function is recursive*)
let addrec x t =
  let rec hasx (T{t;_}) = match t with
    | TVar{x=y;_} ->  x = y
    | TApp{t1;t2;_} -> hasx t1 || hasx t2
    | TLam{x=y;t1;_} -> if x = y then false else hasx t1
    | TIf{t1;t2;_} -> hasx t1 || hasx t2
    | TMatch{cls;_} -> List.exists (fun (_,te) -> hasx te) cls
    | TCont _ -> failwith "Continuation in hasx"
    | TVal _ -> false in

  if hasx t then
    mkapp ~t1:fix ~t2:(mklam ~x ~t1:t)
  else t

(** Make a sequence of lambdas for a multi-parameter function *)
let rec mkfun params body = match params with
  | x::xs -> mklam ~x ~t1:(mkfun xs body)
  | [] -> body

(** Make a sequence of applications for a multi-parameter function *)
let mkapps func args =
  let rec recurse args = match args with
    | t::ts -> mkapp ~t1:(recurse ts) ~t2:t
    | [] -> func
  in recurse (List.rev args)

(** Make a sequence of applications for a multi-parameter value function *)
let mkvalapps valfunc args = mkapps (tm_of_val' valfunc) args

(** Construct a sequence of applications creating a tuple (Tuple constructor
    takes the elements of the tuple in reverse order) *)
let mktup args =
  let rec recurse count args = match args with
    | a::args -> mkapp ~t1:(recurse (count+1) args) ~t2:a
    | [] -> tm_of_val' (VTup{np=count;varr=[||]})
  in
  recurse 0 args

(** Construct a sequence of applications creating a record (Record constructor
    takes the elements of the tuple in reverser order) *)
let mkrecord args =
  let rec recurse keys args = match args with
    | (k,t)::args -> mkapp ~t1:(recurse (k::keys) args) ~t2:t
    | [] -> tm_of_val' (VRec{pls=keys;rls=[]}) in
  recurse [] args

(** Construct a sequence of applications creating a list *)
let rec mklist args = match args with
  | a::args -> mkapp
                 ~t1:(mkapp
                        ~t1:(tm_of_val' (VCons{v1=None}))
                        ~t2:a)
                 ~t2:(mklist args)
  | [] -> tm_of_val' (VList{vls=[]})
