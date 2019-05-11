(** Convert terms and patterns to pretty printed strings *)

open Ast
open Pattern
open Format
open Utils

(** Convert patterns to strings *)
let rec string_of_pat = function
  | PVar(x)    -> x
  | PUnit      -> "()"
  | PChar(c)   -> String.make 1 c
  | PString(s) -> Printf.sprintf "\"%s\"" s
  | PInt(i)    -> string_of_int i
  | PFloat(f)  -> string_of_float f

  | PRec(pls) ->
    Printf.sprintf "{%s}"
      (String.concat "," (List.map (fun (s,p) ->
           let p = string_of_pat p in
           if s = p then
             s
           else
             Printf.sprintf "%s:%s" s p)
           pls))

  | PList(pls)   -> Printf.sprintf "[%s]"
                        (String.concat "," (List.map string_of_pat pls))
  | PTup(pls)    -> Printf.sprintf "(%s)"
                        (String.concat "," (List.map string_of_pat pls))
  | PCons(p1,p2) -> Printf.sprintf "%s:%s"
                        (string_of_pat p1) (string_of_pat p2)

(** Precedence constants for printing *)
type prec =
  | MATCH
  | LAM
  | SEMICOLON
  | IF
  | TUP
  | APP
  | ATOM

(** Simple enum used in the concat function in string_of_tm *)
type sep =
  | SPACE
  | COMMA

(* If the argument is a complete construction of a record, tuple, or list,
   return the constructor itself alongside a list of all the arguments.
   Otherwise, return nothing. Used for pretty printing records, tuples, and
   lists. *)
let compl_constr tm =
  let rec recurse acc tm = match tm with
    | TApp(_,t1,t2) -> recurse (t2::acc) t1
    | TVal(_,(VRec(strs,[]) as v))
      when List.length strs = List.length acc -> Some (v,acc)
    | TVal(_,(VTup(i,_) as v))
      when i = List.length acc -> Some (v,acc)
    | TVal(_,(VList([]) as v)) -> Some (v,acc)
    | _ -> None
  in recurse [] tm

(* Function for concatenating a list of fprintf calls using a given
     separator. *)
let rec concat fmt (sep, ls) = match ls with
  | [] -> ()
  | [f] -> f fmt
  | f :: ls -> match sep with
    | SPACE -> fprintf fmt "%t@ %a" f concat (sep, ls)
    | COMMA -> fprintf fmt "%t,@,%a" f concat (sep, ls)

(** Convert terms to strings. *)
let string_of_tm
    ?(debruijn    = false)
    ?(labels      = false)
    ?(closure_env = false)
    ?(pretty      = true)
    ?(indent      = 2)
    ?(max_indent  = 68)
    ?(margin      = 80)
    t =

  (* Set right margin and maximum indentation *)
  pp_set_margin str_formatter margin;
  pp_set_max_indent str_formatter max_indent;

  let rec recurse fmt (prec, t) =

    (* Print an environment *)
    let print_env fmt env =
      if closure_env then
        let inner = List.mapi (fun i t ->
            (fun fmt ->
               fprintf fmt "%d -> %a" i recurse (MATCH, t))) env in
        fprintf fmt "[@[<hov 0>%a@]]" concat (COMMA,inner)
      else
        fprintf fmt "" in

    (* Function for bare printing (no syntactic sugar) *)
    let bare fmt t = match t with

      | TLam(_,x,t1) ->
        fprintf fmt "@[<hov %d>lam %s.@ %a@]" indent x recurse (MATCH, t1)

      | TIf(_,t1,t2) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>if . then@ %a@]\
                     @ \
                     @[<hov %d>else@ %a@]\
                     @]"
          indent recurse (MATCH, t1)
          indent recurse (MATCH, t2)

      | TMatch(_,cases) ->
        let inner = List.map (fun (p,t1) ->
            (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" indent
                (string_of_pat p) recurse (LAM, t1)))
            cases in
        fprintf fmt "@[<hov %d>match@ .@ with@ @[<hv 0>%a@]@]"
          indent
          concat (SPACE,inner)

      | TApp(_,t1,(TApp _ as t2)) ->
        fprintf fmt "@[<hv 0>%a@ %a@]" recurse (APP, t1) recurse (ATOM, t2)

      | TApp(_,t1,t2) ->
        fprintf fmt "@[<hv 0>%a@ %a@]" recurse (APP, t1) recurse (APP, t2)

      | TVar({var_label;_},x,n) ->
        let vl = if debruijn then Printf.sprintf "#%d" n else "" in
        let d = if labels then Printf.sprintf "|%d" var_label else "" in
        if labels || debruijn then
          fprintf fmt "<%s%s%s>" x vl d
        else fprintf fmt "%s" x

      | TVal(_,v) -> match v with

        | VClos(x,t1,env) ->
            fprintf fmt "@[<hov %d>clos%a %s.@ %a@]"
              indent print_env env x recurse (MATCH, t1)

        | VClosIf(t1,t2,env) ->
          fprintf fmt "@[<hv 0>\
                       @[<hov %d>if%a . then@ %a@]\
                       @ \
                       @[<hov %d>else@ %a@]\
                       @]"
            indent print_env env
            recurse (MATCH, t1)
            indent recurse (MATCH, t2)

        | VClosMatch(cases,env) ->
          let inner = List.map (fun (p,t1) ->
              (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" indent
                  (string_of_pat p) recurse (LAM, t1)))
              cases in
          fprintf fmt "@[<hov %d>match%a@ .@ with@ @[<hv 0>%a@]@]"
            indent print_env env
            concat (SPACE,inner)

        | VFix -> fprintf fmt "fix"

        | VUnit -> fprintf fmt "()"

        | VBool(b)      -> fprintf fmt "%B" b
        | VNot          -> fprintf fmt "not"
        | VAnd(None)    -> fprintf fmt "and"
        | VAnd(Some(v)) -> fprintf fmt "and(%B)" v
        | VOr(None)     -> fprintf fmt "or"
        | VOr(Some(v))  -> fprintf fmt "or(%B)" v

        | VChar(c)      -> fprintf fmt "%C" c

        | VString(s)    -> fprintf fmt "%S" s

        | VInt(v)       -> fprintf fmt "%d" v
        | VMod(None)    -> fprintf fmt "mod"
        | VMod(Some(v)) -> fprintf fmt "mod(%d)" v
        | VSll(None)    -> fprintf fmt "sll"
        | VSll(Some(v)) -> fprintf fmt "sll(%d)" v
        | VSrl(None)    -> fprintf fmt "srl"
        | VSrl(Some(v)) -> fprintf fmt "srl(%d)" v
        | VSra(None)    -> fprintf fmt "sra"
        | VSra(Some(v)) -> fprintf fmt "sra(%d)" v

        | VFloat(v)     -> fprintf fmt "%f" v
        | VLog          -> fprintf fmt "log"

        | VAdd(Some(VInt(v)))   -> fprintf fmt "add(%d)" v
        | VAdd(Some(VFloat(v))) -> fprintf fmt "add(%f)" v
        | VAdd(None)            -> fprintf fmt "add"
        | VAdd _                -> failwith "Not supported"
        | VSub(Some(VInt(v)))   -> fprintf fmt "sub(%d)" v
        | VSub(Some(VFloat(v))) -> fprintf fmt "sub(%f)" v
        | VSub(None)            -> fprintf fmt "sub"
        | VSub _                -> failwith "Not supported"
        | VMul(Some(VInt(v)))   -> fprintf fmt "mul(%d)" v
        | VMul(Some(VFloat(v))) -> fprintf fmt "mul(%f)" v
        | VMul(None)            -> fprintf fmt "mul"
        | VMul _                -> failwith "Not supported"
        | VDiv(Some(VInt(v)))   -> fprintf fmt "div(%d)" v
        | VDiv(Some(VFloat(v))) -> fprintf fmt "div(%f)" v
        | VDiv(None)            -> fprintf fmt "div"
        | VDiv _                -> failwith "Not supported"
        | VNeg                  -> fprintf fmt "neg"
        | VLt(Some(VInt(v)))    -> fprintf fmt "lt(%d)" v
        | VLt(Some(VFloat(v)))  -> fprintf fmt "lt(%f)" v
        | VLt(None)             -> fprintf fmt "lt"
        | VLt _                 -> failwith "Not supported"
        | VLeq(Some(VInt(v)))   -> fprintf fmt "leq(%d)" v
        | VLeq(Some(VFloat(v))) -> fprintf fmt "leq(%f)" v
        | VLeq(None)            -> fprintf fmt "leq"
        | VLeq _                -> failwith "Not supported"
        | VGt(Some(VInt(v)))    -> fprintf fmt "gt(%d)" v
        | VGt(Some(VFloat(v)))  -> fprintf fmt "gt(%f)" v
        | VGt(None)             -> fprintf fmt "gt"
        | VGt _                 -> failwith "Not supported"
        | VGeq(Some(VInt(v)))   -> fprintf fmt "geq(%d)" v
        | VGeq(Some(VFloat(v))) -> fprintf fmt "geq(%f)" v
        | VGeq(None)            -> fprintf fmt "geq"
        | VGeq _                -> failwith "Not supported"

        | VEq(None)     -> fprintf fmt "eq"
        | VEq(Some(v))  -> fprintf fmt "eq(%a)" recurse (MATCH, tm_of_val v)
        | VNeq(None)    -> fprintf fmt "neq"
        | VNeq(Some(v)) -> fprintf fmt "neq(%a)" recurse (MATCH, tm_of_val v)

        | VNormal(None,None)       -> fprintf fmt "normal"
        | VNormal(Some f1,None)    -> fprintf fmt "normal(%f)" f1
        | VNormal(Some f1,Some f2) -> fprintf fmt "normal(%f,%f)" f1 f2
        | VNormal _                -> failwith "Not supported"

        | VUniform(None,None)       -> fprintf fmt "uniform"
        | VUniform(Some f1,None)    -> fprintf fmt "uniform(%f)" f1
        | VUniform(Some f1,Some f2) -> fprintf fmt "uniform(%f,%f)" f1 f2
        | VUniform _                -> failwith "Not supported"

        | VGamma(None,None)       -> fprintf fmt "gamma"
        | VGamma(Some f1,None)    -> fprintf fmt "gamma(%f)" f1
        | VGamma(Some f1,Some f2) -> fprintf fmt "gamma(%f,%f)" f1 f2
        | VGamma _                -> failwith "Not supported"

        | VExp(None)   -> fprintf fmt "exp"
        | VExp(Some f) -> fprintf fmt "exp(%f)" f

        | VBern(None)   -> fprintf fmt "bern"
        | VBern(Some f) -> fprintf fmt "bern(%f)" f

        | VLogPdf(None) -> fprintf fmt "logpdf"
        | VLogPdf(Some v) -> fprintf fmt "logpdf(%a)"
                               recurse (MATCH,tm_of_val v)

        | VSample -> fprintf fmt "sample"

        | VTup(i,varr) ->
          let inner = Array.map (fun v ->
              (fun fmt -> fprintf fmt "%a" recurse (APP, tm_of_val v)))
              varr in
          let inner = Array.to_list inner
                      @ replicate
                        (i - Array.length inner)
                        (fun fmt -> fprintf fmt ".") in
          fprintf fmt "@[<hov 0>%a@]" concat (COMMA,inner)

        | VRec(strs,sm) ->
          let inner = List.map (fun (k, v) ->
              (fun fmt ->
                 fprintf fmt "%s:%a" k recurse (MATCH, tm_of_val v))) sm in
          let inner = inner
                      @ List.map
                        (fun k -> (fun fmt -> fprintf fmt "%s:." k)) strs in
          fprintf fmt "{@[<hov 0>%a@]}" concat (COMMA,inner)

        | VList(ls) ->
          let inner = List.map (fun v ->
              (fun fmt ->
                 fprintf fmt "%a" recurse (MATCH, tm_of_val v))) ls in
          fprintf fmt "[@[<hov 0>%a@]]"
            concat (COMMA,inner)

        | VRecProj(x) -> fprintf fmt "(.).%s" x
        | VTupProj(i) -> fprintf fmt "(.).%d" i

        | VUtest(Some v) -> fprintf fmt "utest(%a)"
                              recurse (MATCH, tm_of_val v)
        | VUtest _       -> fprintf fmt "utest"

        | VConcat(None)   -> fprintf fmt "concat"
        | VConcat(Some v) -> fprintf fmt "concat(%a)"
                               recurse (MATCH, tm_of_val v)

        | VWeight -> fprintf fmt "weight"

        | VResamp(None,None)      -> fprintf fmt "resample"
        | VResamp(Some v,None)    -> fprintf fmt "resample(%a)"
                                       recurse (MATCH, tm_of_val v)
        | VResamp(Some v,Some b1) -> fprintf fmt "resample(%a,%B)"
                                       recurse (APP, tm_of_val v)
                                       b1
        | VResamp _ -> failwith "Invalid resample"

    in

    (* Syntactic sugar printing *)
    let sugar fmt t = match t with

      (* Record projection application *)
      | TApp(_,TVal(_,VRecProj(x)),t1) ->
        fprintf fmt "%a.%s" recurse (APP, t1) x

      (* Tuple projection applications *)
      | TApp(_,TVal(_,VTupProj(i)),t1) ->
        fprintf fmt "%a.%d" recurse (APP, t1) i

      (* If applications *)
      | TApp(_,TIf(_,t1,t2),t) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>if %a then@ %a@]\
                     @ \
                     @[<hov %d>else@ %a@]\
                     @]"
          indent recurse (MATCH, t) recurse (MATCH, t1)
          indent recurse (MATCH, t2)

      (* Match applications *)
      | TApp(_,TMatch(_,cases),t) ->
        let inner = List.map (fun (p,t1) ->
            (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" indent
                (string_of_pat p) recurse (LAM, t1)))
            cases in
        fprintf fmt "@[<hov %d>match@ %a@ with@ @[<hv 0>%a@]@]"
          indent
          recurse (MATCH, t)
          concat (SPACE,inner)

      (* Sequencing (right associative) *)
      | TApp(_,TLam(_,"_",t2),
              (TApp(_,TLam(_,"_",_),_) as t1)) ->
        fprintf fmt "@[<hv 0>%a;@ %a@]"
          recurse (IF, t1) recurse (MATCH, t2)
      | TApp(_,TLam(_,"_",t2),t1) ->
        fprintf fmt "@[<hv 0>%a;@ %a@]"
          recurse (SEMICOLON, t1) recurse (MATCH, t2)

      (* Let expressions *)
      | TApp(_,TLam(_,x,t1),t2) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>let %s =@ %a in@]\
                     @ %a@]"
          indent x recurse (MATCH, t2) recurse (MATCH, t1)

      | _ -> match compl_constr t with

        (* Records *)
        | Some (VRec(strs,_),args) ->
          let inner = List.map (fun (k, t1) ->
              (fun fmt -> fprintf fmt "%s:%a" k recurse (MATCH, t1)))
              (List.combine strs args) in
          fprintf fmt "{@[<hov 0>%a@]}"
            concat (COMMA,inner)

        (* Tuples *)
        | Some (VTup _,args) ->
          let inner = List.map (fun t1 ->
              (fun fmt -> fprintf fmt "%a" recurse (APP, t1))) args in
          fprintf fmt "@[<hov 0>%a@]"
            concat (COMMA,inner)

        (* Lists *)
        | Some (VList _,args) ->
          let inner = List.map (fun t1 ->
              (fun fmt -> fprintf fmt "%a" recurse (MATCH, t1))) args in
          fprintf fmt "[@[<hov 0>%a@]]"
            concat (COMMA,inner)

        | Some _ -> failwith "Not possible"

        (* Otherwise, fall back to bare printing *)
        | None -> bare fmt t
    in

    (* Check if this term should be parenthesized *)
    let paren = match t with

      | TApp(_,TLam(_,"_",_),_) when pretty -> prec > SEMICOLON
      | TApp(_,TLam(_,_,_),_)   when pretty -> prec > LAM

      | TMatch _        -> prec > MATCH
      | TLam _          -> prec > LAM
      | TIf _           -> prec > IF
      | TVal(_,VTup _)  -> prec > TUP
      | TApp _          -> prec > APP
      | TVar _ | TVal _ -> prec > ATOM

    in

    let p = if pretty then sugar else bare in

    if labels then
    (* TODO This approach to printing labels omits some labels when using
       pretty printing *)
      match t with
      | TVar _ | TVal _ -> fprintf fmt "%a:%d"   p t (tm_label t)
      | _               -> fprintf fmt "(%a):%d" p t (tm_label t)
    else if paren then
      fprintf fmt "(%a)" p t
    else
      fprintf fmt "%a" p t

  in recurse str_formatter (MATCH, t); flush_str_formatter ()

