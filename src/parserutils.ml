(** Parser utilities *)

open Ast

(** Function for adding fix-point if a function is recursive*)
let addrec x t =

  let rec hasx t = match t with
    | TVar{x=y;_} ->  x = y
    | TApp{t1;t2;_} -> hasx t1 || hasx t2
    | TLam{x=y;t1;_} -> if x = y then false else hasx t1
    | TIf{t1;t2;_} -> hasx t1 || hasx t2
    | TMatch{cls;_} -> List.exists (fun (_,te) -> hasx te) cls

    | TVal _ -> false in

  if hasx t then
    TApp{at=ta;t1=fix;t2=TLam{at=ta;vat=xa;cont=false;x=x;t1=t}}
  else t

(** Make a sequence of lambdas for a multi-parameter function *)
let rec mkfun params body = match params with
  | x::xs -> TLam{at=ta;vat=xa;cont=false;x=x;t1=mkfun xs body}
  | [] -> body

(** Make a sequence of applications for a multi-parameter function *)
let mkapps func args =
  let rec recurse args = match args with
    | t::ts -> TApp{at=ta;t1=recurse ts;t2=t}
    | [] -> func
  in recurse (List.rev args)

(** Make a sequence of applications for a multi-parameter value function *)
let mkvalapps valfunc args = mkapps (TVal{at=ta;v=valfunc}) args

(** Construct a sequence of applications creating a tuple (Tuple constructor
    takes the elements of the tuple in reverse order) *)
let mktup args =
  let rec recurse count args = match args with
    | a::args -> TApp{at=ta;t1=recurse (count+1) args;t2=a}
    | [] -> TVal{at=ta;v=VTup{at=va;np=count;varr=[||]}} in
  recurse 0 args

(** Construct a sequence of applications creating a record (Record constructor
    takes the elements of the tuple in reverser order) *)
let mkrecord args =
  let rec recurse keys args = match args with
    | (k,t)::args -> TApp{at=ta;t1=recurse (k::keys) args;t2=t}
    | [] -> TVal{at=ta;v=VRec{at=va;pls=keys;rls=[]}} in
  recurse [] args

(** Construct a sequence of applications creating a list *)
let rec mklist args = match args with
  | a::args -> TApp{at=ta;
                    t1=TApp{at=ta;
                            t1=TVal{at=ta;v=VCons{at=va;v1=None}};
                            t2=a};
                    t2=mklist args}
  | [] -> TVal{at=ta;v=VList{at=va;vls=[]}}
