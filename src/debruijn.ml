(** Debruijn conversion of terms *)

open Pattern
open Ast

(** Extends an environment used in debruijn conversion with the identifiers
    found in the given pattern *)
let rec patenv env pat = match pat with
  | PVar(s)         -> s :: env
  | PRec((_,p)::ps) -> patenv (patenv env p) (PRec(ps))
  | PRec([])        -> env
  | PList(p::ps)    -> patenv (patenv env p) (PList(ps))
  | PList([])       -> env
  | PTup(p::ps)     -> patenv (patenv env p) (PTup(ps))
  | PTup([])        -> env
  | PCons(p1,p2)    -> patenv (patenv env p1) p2

  | PUnit     | PChar _
  | PString _ | PInt _  | PFloat _ -> env

(** Add debruijn indices to a term *)
let rec debruijn env t = match t with
  | TVar({x;_} as t) ->
    let rec find env n = match env with
      | y::ee -> if y = x then n else find ee (n+1)
      | [] -> failwith ("Unknown variable in debruijn conversion: " ^ x)
    in TVar{t with i=find env 0}

  | TApp{at;t1;t2;_} -> TApp{at;t1=debruijn env t1; t2=debruijn env t2}

  | TLam({x;t1;_} as t) -> TLam{t with t1=debruijn (x::env) t1}

  | TCont({x;t1;_} as t) -> TCont{t with t1=debruijn (x::env) t1}

  | TIf{at;t1;t2} -> TIf{at;t1=debruijn env t1;t2=debruijn env t2}

  | TMatch{at;cls} ->
    TMatch{at;cls=Utils.map (fun (p,t) -> p,debruijn (patenv env p) t) cls}

  | TVal _ -> t

