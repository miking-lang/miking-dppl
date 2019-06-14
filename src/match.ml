(** Pattern matching *)

open Pattern
open Ast

(** If the pattern matches the given value, return the extended environment
    where the variables in the pattern are bound to the corresponding terms in
    the value.
    NOTE: Follows the same traversal order of the pattern as in
    the patenv function during Debruijn conversion to get the correct debruijn
    indices. *)
let rec match_case env pattern value = match pattern,value with
  | PVar _,v -> Some(tm_of_val v :: env)

  | PRec((k,p)::ps),(VRec{pls=[];rls;_} as v) ->
    (match List.assoc_opt k rls with
     | Some v1 ->
       (match match_case env p v1 with
        | Some env -> match_case env (PRec(ps)) v
        | None     -> None)
     | None -> None)
  | PRec([]),VRec _ -> Some env
  | PRec _,_        -> None

  | PList(p::ps),VList{at;vls=v::vs;_} ->
    (match match_case env p v with
     | Some env -> match_case env (PList(ps)) (VList{at;vls=vs})
     | None     -> None)
  | PList([]),VList{vls=[];_} -> Some env
  | PList _,_              -> None

  | PTup(ps),VTup{np=0;varr;_} ->
    let rec fold env ps i = match ps with
      | p::ps when i < Array.length varr ->
        (match match_case env p varr.(i) with
         | Some env -> fold env ps (i + 1)
         | None     -> None)
      | [] when i = Array.length varr -> Some env
      | _                             -> None
    in fold env ps 0
  | PTup _,_ -> None

  | PCons(p1,p2),VList{at;vls=v::vs;_} ->
    (match match_case env p1 v with
     | Some env -> match_case env p2 (VList{at;vls=vs})
     | None     -> None)
  | PCons _,_ -> None

  | PUnit,        VUnit _                      -> Some env
  | PUnit,        _                            -> None
  | PChar(c1),    VChar{c=c2;_}   when c1 = c2 -> Some env
  | PChar _,      _                            -> None
  | PString(s1),  VString{s=s2;_} when s1 = s2 -> Some env
  | PString _,    _                            -> None
  | PInt(i1),     VInt{i=i2;_}    when i1 = i2 -> Some env
  | PInt _,       _                            -> None
  | PFloat(f1),   VFloat{f=f2;_}  when f1 = f2 -> Some env
  | PFloat _,     _                            -> None

