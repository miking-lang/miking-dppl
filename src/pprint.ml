open Ast
open Printf
open Utils

(* Pretty print constants *)
let rec pprint_const c =
  match c with
  (* MCore Intrinsic Booleans *)
  | CBool(b) -> string_of_bool b
  | CNot -> "not"
  | CAnd(None) -> "and"
  | CAnd(Some(v)) -> "and(" ^ string_of_bool v ^ ")"
  | COr(None) -> "or"
  | COr(Some(v)) -> "or(" ^ string_of_bool v ^ ")"
  (* MCore Intrinsic Integers *)
  | CInt(v) -> sprintf "%d" v
  | CModi(None) -> "modi"
  | CModi(Some(v)) -> sprintf "modi(%d)" v
  | CSlli(None) -> "slli"
  | CSlli(Some(v)) -> sprintf "slli(%d)" v
  | CSrli(None) -> "srli"
  | CSrli(Some(v)) -> sprintf "srli(%d)" v
  | CSrai(None) -> "srai"
  | CSrai(Some(v)) -> sprintf "srai(%d)" v
  (* MCore intrinsic: Floating-point number constant and operations *)
  | CFloat(v) -> sprintf "%f" v
  | CLog -> "log"
  (* Mcore intrinsic: Polymorphic integer and floating-point numbers *)
  | CAdd(Some(CInt(v))) -> sprintf "add(%d)" v
  | CAdd(Some(CFloat(v))) -> sprintf "add(%f)" v
  | CAdd(None) -> "add"
  | CSub(Some(CInt(v))) -> sprintf "sub(%d)" v
  | CSub(Some(CFloat(v))) -> sprintf "sub(%f)" v
  | CSub(None) -> "sub"
  | CMul(Some(CInt(v))) -> sprintf "mul(%d)" v
  | CMul(Some(CFloat(v))) -> sprintf "mul(%f)" v
  | CMul(None) -> "mul"
  | CDiv(Some(CInt(v))) -> sprintf "div(%d)" v
  | CDiv(Some(CFloat(v))) -> sprintf "div(%f)" v
  | CDiv(None) -> "div"
  | CNeg -> "neg"
  | CLt(Some(CInt(v))) -> sprintf "lt(%d)" v
  | CLt(Some(CFloat(v))) -> sprintf "lt(%f)" v
  | CLt(None) -> "lt"
  | CLeq(Some(CInt(v))) -> sprintf "leq(%d)" v
  | CLeq(Some(CFloat(v))) -> sprintf "leq(%f)" v
  | CLeq(None) -> "leq"
  | CGt(Some(CInt(v))) -> sprintf "gt(%d)" v
  | CGt(Some(CFloat(v))) -> sprintf "gt(%f)" v
  | CGt(None) -> "gt"
  | CGeq(Some(CInt(v))) -> sprintf "geq(%d)" v
  | CGeq(Some(CFloat(v))) -> sprintf "geq(%f)" v
  | CGeq(None) -> "geq"

  (* Ragnar polymorpic temps *)
  | CEq(None) -> "polyeq"
  | CEq(Some(v)) -> "polyeq(" ^ (pprint v) ^ ")"
  | CNeq(None) -> "polyneq"
  | CNeq(Some(v)) -> "polyneq(" ^ (pprint v) ^ ")"

  | _ -> failwith "Unsupported const in print_const"

(* Pretty print a term. *)
and pprint t =

  let rec pprint' prec t =
    let p = pprint'' t in
    let paren = match t with
      | TmLam _ | TmClos _ -> prec > 0
      | TmApp _ | TmUtest _ -> prec > 1
      | TmVar _ | TmConst _ | TmFix _
      | TmIfexp _ | TmNop _ | TmRec _ | TmProj _ -> false
    in if paren then "(" ^ p ^ ")" else p

  and pprint'' t =
    match t with
    | TmVar(_,x,n) -> x ^ "#" ^ string_of_int n
    | TmLam(_,x,t1) -> "lam " ^ x ^ ". " ^ pprint' 0 t1
    | TmClos(_,x,t,_) -> "clos " ^ x ^ ". " ^ pprint' 0 t
    | TmApp(_,t1,(TmApp _ as t2)) -> pprint' 1 t1 ^ " " ^ pprint' 2 t2
    | TmApp(_,t1,t2) -> pprint' 1 t1 ^ " " ^ pprint' 1 t2
    | TmConst(_,c) -> pprint_const c
    | TmFix _ -> "fix"
    | TmIfexp(_,None,_) -> "ifexp"
    | TmIfexp(_,Some(g),Some(t2)) ->
      "ifexp(" ^ string_of_bool g ^ "," ^ pprint' 0 t2 ^ ")"
    | TmIfexp(_,Some(g),_) -> "ifexp(" ^ string_of_bool g ^ ")"
    | TmUtest(_,t1,t2) -> "utest " ^ pprint' 2 t1 ^ " " ^ pprint' 2 t2
    | TmNop _ -> "Nop"

    | TmRec(_,sm) ->
      let binds = StrMap.bindings sm in
      let inner = List.map (fun (k, t1) -> k ^ ":" ^ pprint' 0 t1) binds in
      "{ " ^ (String.concat (",") inner) ^ " }"

    | TmProj(_,t1,x) -> pprint' 2 t1 ^ "." ^ x

  in pprint' 0 t

(* Function for pretty printing terms with labels *)
let rec pprintl = function
  | TmVar({label;var_label;_},x,_) ->
    x ^ "|" ^ (string_of_int var_label) ^ ":" ^ (string_of_int label)
  | TmLam({label;var_label;_},x,t1) ->
    "(lam " ^ x ^ "|" ^ (string_of_int var_label) ^ ". " ^
    (pprintl t1) ^ "):" ^ (string_of_int label)
  | TmApp({label;_},t1,t2) -> "(" ^ pprintl t1 ^ " " ^ pprintl t2
                                ^ "):" ^ (string_of_int label)
  | TmConst({label;_},c) -> pprint_const c ^ ":" ^ (string_of_int label)
  | TmIfexp({label;_},_,_) -> "if:" ^ (string_of_int label)
  | TmFix({label;_}) -> "fix:" ^ (string_of_int label)

  (* Records *)
  | TmRec({label;_},sm) ->
      let binds = StrMap.bindings sm in
      let inner = List.map (fun (k, t1) -> k ^ ":" ^ pprintl t1) binds in
      "{ " ^ (String.concat (",") inner) ^ " }:"
      ^ (string_of_int label)
  | TmProj({label;_},t1,x) -> pprintl t1 ^ "." ^ x ^ ":" ^
                              (string_of_int label)

  | TmNop{label;_} -> "Nop" ^ ":" ^ (string_of_int label)

  | TmClos _ -> "Closure"

  | TmUtest _ -> failwith "Not supported"

(* Pretty prints the environment *)
let pprint_env env =
  "[" ^ (List.mapi (fun i t -> sprintf " %d -> " i ^ pprint t) env
            |> String.concat ",") ^ "]"
