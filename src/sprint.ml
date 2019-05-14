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

(** Global configuration for prints. Needed because of the unwieldy interface
    to the Format module *)
let ref_debruijn    = ref false
let ref_labels      = ref false
let ref_closure_env = ref false
let ref_pretty      = ref false
let ref_indent      = ref 2

(** Simple enum used in the concat function in string_of_tm *)
type sep =
  | SPACE
  | COMMA

(* Function for concatenating a list of fprintf calls using a given
     separator. *)
let rec concat fmt (sep, ls) = match ls with
  | [] -> ()
  | [f] -> f fmt
  | f :: ls -> match sep with
    | SPACE -> fprintf fmt "%t@ %a" f concat (sep, ls)
    | COMMA -> fprintf fmt "%t,@,%a" f concat (sep, ls)

(* If the argument is a complete construction of a record, tuple, or list,
   return the constructor itself alongside a list of all the arguments.
   Otherwise, return nothing. Used for pretty printing records, tuples, and
   lists. *)
let compl_constr tm =
  let rec recurse acc tm = match tm with
    | TApp(_,t1,t2) -> recurse (t2::acc) t1
    | TVal(VRec(_,strs,[]) as v)
      when List.length strs = List.length acc -> Some (v,acc)
    | TVal(VTup(_,i,_) as v)
      when i = List.length acc -> Some (v,acc)
    | TVal(VList(_,[]) as v) -> Some (v,acc)
    | _ -> None
  in recurse [] tm

let rec print_tm fmt (prec, t) =

  (* Function for bare printing (no syntactic sugar) *)
  let bare fmt t = match t with

    | TLam(_,x,t1) ->
      fprintf fmt "@[<hov %d>lam %s.@ %a@]" !ref_indent x print_tm (MATCH, t1)

    | TIf(_,t1,t2) ->
      fprintf fmt "@[<hv 0>\
                   @[<hov %d>if . then@ %a@]\
                   @ \
                   @[<hov %d>else@ %a@]\
                   @]"
        !ref_indent print_tm (MATCH, t1)
        !ref_indent print_tm (MATCH, t2)

    | TMatch(_,cases) ->
      let inner = List.map (fun (p,t1) ->
          (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
              (string_of_pat p) print_tm (LAM, t1)))
          cases in
      fprintf fmt "@[<hov %d>match@ .@ with@ @[<hv 0>%a@]@]"
        !ref_indent
        concat (SPACE,inner)

    | TApp(_,t1,(TApp _ as t2)) ->
      fprintf fmt "@[<hv 0>%a@ %a@]" print_tm (APP, t1) print_tm (ATOM, t2)

    | TApp(_,t1,t2) ->
      fprintf fmt "@[<hv 0>%a@ %a@]" print_tm (APP, t1) print_tm (APP, t2)

    | TVar({var_label;_},x,n) ->
      let vl = if !ref_debruijn then Printf.sprintf "#%d" n else "" in
      let d = if !ref_labels then Printf.sprintf "|%d" var_label else "" in
      if !ref_labels || !ref_debruijn then
        fprintf fmt "<%s%s%s>" x vl d
      else fprintf fmt "%s" x

    | TVal(v) -> match v with

      | VClos(_,x,t1,env) ->
        fprintf fmt "@[<hov %d>clos%a %s.@ %a@]"
          !ref_indent print_env env x print_tm (MATCH, t1)

      | VClosIf(_,t1,t2,env) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>if%a . then@ %a@]\
                     @ \
                     @[<hov %d>else@ %a@]\
                     @]"
          !ref_indent print_env env
          print_tm (MATCH, t1)
          !ref_indent print_tm (MATCH, t2)

      | VClosMatch(_,cases,env) ->
        let inner = List.map (fun (p,t1) ->
            (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
                (string_of_pat p) print_tm (LAM, t1)))
            cases in
        fprintf fmt "@[<hov %d>match%a@ .@ with@ @[<hv 0>%a@]@]"
          !ref_indent print_env env
          concat (SPACE,inner)

      | VFix _ -> fprintf fmt "fix"

      | VUnit _ -> fprintf fmt "()"

      | VBool(_,b)      -> fprintf fmt "%B" b
      | VNot _          -> fprintf fmt "not"
      | VAnd(_,None)    -> fprintf fmt "and"
      | VAnd(_,Some(v)) -> fprintf fmt "and(%B)" v
      | VOr(_,None)     -> fprintf fmt "or"
      | VOr(_,Some(v))  -> fprintf fmt "or(%B)" v

      | VChar(_,c)      -> fprintf fmt "%C" c

      | VString(_,s)    -> fprintf fmt "%S" s

      | VInt(_,v)       -> fprintf fmt "%d" v
      | VMod(_,None)    -> fprintf fmt "mod"
      | VMod(_,Some(v)) -> fprintf fmt "mod(%d)" v
      | VSll(_,None)    -> fprintf fmt "sll"
      | VSll(_,Some(v)) -> fprintf fmt "sll(%d)" v
      | VSrl(_,None)    -> fprintf fmt "srl"
      | VSrl(_,Some(v)) -> fprintf fmt "srl(%d)" v
      | VSra(_,None)    -> fprintf fmt "sra"
      | VSra(_,Some(v)) -> fprintf fmt "sra(%d)" v

      | VFloat(_,v) -> fprintf fmt "%f" v
      | VLog _      -> fprintf fmt "log"

      | VAdd(_,Some(VInt(_,v)))   -> fprintf fmt "add(%d)" v
      | VAdd(_,Some(VFloat(_,v))) -> fprintf fmt "add(%f)" v
      | VAdd(_,None)              -> fprintf fmt "add"
      | VAdd _                    -> failwith "Not supported"
      | VSub(_,Some(VInt(_,v)))   -> fprintf fmt "sub(%d)" v
      | VSub(_,Some(VFloat(_,v))) -> fprintf fmt "sub(%f)" v
      | VSub(_,None)              -> fprintf fmt "sub"
      | VSub _                    -> failwith "Not supported"
      | VMul(_,Some(VInt(_,v)))   -> fprintf fmt "mul(%d)" v
      | VMul(_,Some(VFloat(_,v))) -> fprintf fmt "mul(%f)" v
      | VMul(_,None)              -> fprintf fmt "mul"
      | VMul _                    -> failwith "Not supported"
      | VDiv(_,Some(VInt(_,v)))   -> fprintf fmt "div(%d)" v
      | VDiv(_,Some(VFloat(_,v))) -> fprintf fmt "div(%f)" v
      | VDiv(_,None)              -> fprintf fmt "div"
      | VDiv _                    -> failwith "Not supported"
      | VNeg _                    -> fprintf fmt "neg"
      | VLt(_,Some(VInt(_,v)))    -> fprintf fmt "lt(%d)" v
      | VLt(_,Some(VFloat(_,v)))  -> fprintf fmt "lt(%f)" v
      | VLt(_,None)               -> fprintf fmt "lt"
      | VLt _                     -> failwith "Not supported"
      | VLeq(_,Some(VInt(_,v)))   -> fprintf fmt "leq(%d)" v
      | VLeq(_,Some(VFloat(_,v))) -> fprintf fmt "leq(%f)" v
      | VLeq(_,None)              -> fprintf fmt "leq"
      | VLeq _                    -> failwith "Not supported"
      | VGt(_,Some(VInt(_,v)))    -> fprintf fmt "gt(%d)" v
      | VGt(_,Some(VFloat(_,v)))  -> fprintf fmt "gt(%f)" v
      | VGt(_,None)               -> fprintf fmt "gt"
      | VGt _                     -> failwith "Not supported"
      | VGeq(_,Some(VInt(_,v)))   -> fprintf fmt "geq(%d)" v
      | VGeq(_,Some(VFloat(_,v))) -> fprintf fmt "geq(%f)" v
      | VGeq(_,None)              -> fprintf fmt "geq"
      | VGeq _                    -> failwith "Not supported"

      | VEq(_,None)     -> fprintf fmt "eq"
      | VEq(_,Some(v))  -> fprintf fmt "eq(%a)" print_tm (MATCH, tm_of_val v)
      | VNeq(_,None)    -> fprintf fmt "neq"
      | VNeq(_,Some(v)) -> fprintf fmt "neq(%a)" print_tm (MATCH, tm_of_val v)

      | VNormal(_,None,None)       -> fprintf fmt "normal"
      | VNormal(_,Some f1,None)    -> fprintf fmt "normal(%f)" f1
      | VNormal(_,Some f1,Some f2) -> fprintf fmt "normal(%f,%f)" f1 f2
      | VNormal _                  -> failwith "Not supported"

      | VUniform(_,None,None)       -> fprintf fmt "uniform"
      | VUniform(_,Some f1,None)    -> fprintf fmt "uniform(%f)" f1
      | VUniform(_,Some f1,Some f2) -> fprintf fmt "uniform(%f,%f)" f1 f2
      | VUniform _                  -> failwith "Not supported"

      | VGamma(_,None,None)       -> fprintf fmt "gamma"
      | VGamma(_,Some f1,None)    -> fprintf fmt "gamma(%f)" f1
      | VGamma(_,Some f1,Some f2) -> fprintf fmt "gamma(%f,%f)" f1 f2
      | VGamma _                  -> failwith "Not supported"

      | VExp(_,None)   -> fprintf fmt "exp"
      | VExp(_,Some f) -> fprintf fmt "exp(%f)" f

      | VBern(_,None)   -> fprintf fmt "bern"
      | VBern(_,Some f) -> fprintf fmt "bern(%f)" f

      | VLogPdf(_,None)   -> fprintf fmt "logpdf"
      | VLogPdf(_,Some v) -> fprintf fmt "logpdf(%a)"
                               print_tm (MATCH,tm_of_val v)

      | VSample _ -> fprintf fmt "sample"

      | VTup(_,i,varr) ->
        let inner = Array.map (fun v ->
            (fun fmt -> fprintf fmt "%a" print_tm (APP, tm_of_val v)))
            varr in
        let inner = Array.to_list inner
                    @ replicate
                      (i - Array.length inner)
                      (fun fmt -> fprintf fmt ".") in
        fprintf fmt "@[<hov 0>%a@]" concat (COMMA,inner)

      | VRec(_,strs,sm) ->
        let inner = List.map (fun (k, v) ->
            (fun fmt ->
               fprintf fmt "%s:%a" k print_tm (MATCH, tm_of_val v))) sm in
        let inner = inner
                    @ List.map
                      (fun k -> (fun fmt -> fprintf fmt "%s:." k)) strs in
        fprintf fmt "{@[<hov 0>%a@]}" concat (COMMA,List.rev inner)

      | VList(_,ls) ->
        let inner = List.map (fun v ->
            (fun fmt ->
               fprintf fmt "%a" print_tm (MATCH, tm_of_val v))) ls in
        fprintf fmt "[@[<hov 0>%a@]]"
          concat (COMMA,inner)

      | VCons(_,None)   -> fprintf fmt "cons"
      | VCons(_,Some v) -> fprintf fmt "cons(%a)"
                             print_tm (MATCH, tm_of_val v)

      | VRecProj(_,x) -> fprintf fmt "(.).%s" x
      | VTupProj(_,i) -> fprintf fmt "(.).%d" i

      | VUtest(_,Some v) -> fprintf fmt "utest(%a)"
                              print_tm (MATCH, tm_of_val v)
      | VUtest _         -> fprintf fmt "utest"

      | VConcat(_,None)   -> fprintf fmt "concat"
      | VConcat(_,Some v) -> fprintf fmt "concat(%a)"
                               print_tm (MATCH, tm_of_val v)

      | VWeight _ -> fprintf fmt "weight"

      | VResamp(_,None,None)      -> fprintf fmt "resample"
      | VResamp(_,Some v,None)    -> fprintf fmt "resample(%a)"
                                       print_tm (MATCH, tm_of_val v)
      | VResamp(_,Some v,Some b1) -> fprintf fmt "resample(%a,%B)"
                                       print_tm (APP, tm_of_val v)
                                       b1
      | VResamp _ -> failwith "Invalid resample"

  in

  (* Syntactic sugar printing *)
  let sugar fmt t = match t with

    (* Record projection application *)
    | TApp(_,TVal(VRecProj(_,x)),t1) ->
      fprintf fmt "%a.%s" print_tm (APP, t1) x

    (* Tuple projection applications *)
    | TApp(_,TVal(VTupProj(_,i)),t1) ->
      fprintf fmt "%a.%d" print_tm (APP, t1) i

    (* If applications *)
    | TApp(_,TIf(_,t1,t2),t) ->
      fprintf fmt "@[<hv 0>\
                   @[<hov %d>if %a then@ %a@]\
                   @ \
                   @[<hov %d>else@ %a@]\
                   @]"
        !ref_indent print_tm (MATCH, t) print_tm (MATCH, t1)
        !ref_indent print_tm (MATCH, t2)

    (* Match applications *)
    | TApp(_,TMatch(_,cases),t) ->
      let inner = List.map (fun (p,t1) ->
          (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
              (string_of_pat p) print_tm (LAM, t1)))
          cases in
      fprintf fmt "@[<hov %d>match@ %a@ with@ @[<hv 0>%a@]@]"
        !ref_indent
        print_tm (MATCH, t)
        concat (SPACE,inner)

    (* Sequencing (right associative) *)
    | TApp(_,TLam(_,"_",t2),
           (TApp(_,TLam(_,"_",_),_) as t1)) ->
      fprintf fmt "@[<hv 0>%a;@ %a@]"
        print_tm (IF, t1) print_tm (MATCH, t2)
    | TApp(_,TLam(_,"_",t2),t1) ->
      fprintf fmt "@[<hv 0>%a;@ %a@]"
        print_tm (SEMICOLON, t1) print_tm (MATCH, t2)

    (* Let expressions *)
    | TApp(_,TLam(_,x,t1),t2) ->
      fprintf fmt "@[<hv 0>\
                   @[<hov %d>let %s =@ %a in@]\
                   @ %a@]"
        !ref_indent x print_tm (MATCH, t2) print_tm (MATCH, t1)

    | _ -> match compl_constr t with

      (* Records *)
      | Some (VRec(_,strs,_),args) ->
        let inner = List.map (fun (k, t1) ->
            (fun fmt -> fprintf fmt "%s:%a" k print_tm (MATCH, t1)))
            (List.combine strs args) in
        fprintf fmt "{@[<hov 0>%a@]}"
          concat (COMMA,inner)

      (* Tuples *)
      | Some (VTup _,args) ->
        let inner = List.map (fun t1 ->
            (fun fmt -> fprintf fmt "%a" print_tm (APP, t1))) args in
        fprintf fmt "@[<hov 0>%a@]"
          concat (COMMA,inner)

      (* Lists *)
      | Some (VList _,args) ->
        let inner = List.map (fun t1 ->
            (fun fmt -> fprintf fmt "%a" print_tm (MATCH, t1))) args in
        fprintf fmt "[@[<hov 0>%a@]]"
          concat (COMMA,inner)

      | Some _ -> failwith "Not possible"

      (* Otherwise, fall back to bare printing *)
      | None -> bare fmt t
  in

  (* Check if this term should be parenthesized *)
  let paren = match t with

    | TApp(_,TLam(_,"_",_),_) when !ref_pretty -> prec > SEMICOLON
    | TApp(_,TLam(_,_,_),_)   when !ref_pretty -> prec > LAM

    | TMatch _        -> prec > MATCH
    | TLam _          -> prec > LAM
    | TIf _           -> prec > IF
    | TVal(VTup _)    -> prec > TUP
    | TApp _          -> prec > APP
    | TVar _ | TVal _ -> prec > ATOM

  in

  let p = if !ref_pretty then sugar else bare in

  if !ref_labels then
    (* TODO This approach to printing labels omits some labels when using
       pretty printing *)
    match t with
    | TVar _ | TVal _ -> fprintf fmt "%a:%d"   p t (tm_label t)
    | _               -> fprintf fmt "(%a):%d" p t (tm_label t)
  else if paren then
    fprintf fmt "(%a)" p t
  else
    fprintf fmt "%a" p t

(* Print an environment. The function f handles printing of the constituent
   elements. *)
and print_env fmt env =
  if !ref_closure_env then
    let inner = List.mapi (fun i t ->
        (fun fmt ->
           fprintf fmt "%d -> %a" i print_tm (MATCH, t))) env in
    fprintf fmt "[@[<hov 0>%a@]]" concat (COMMA,inner)
  else
    fprintf fmt ""

(* Setup printer *)
let setup_print
    ?(debruijn       = false)
    ?(labels         = false)
    ?(closure_env    = false)
    ?(pretty         = true)
    ?(indent         = 2)
    ?(max_indent     = 68)
    ?(margin         = 80)
    () =
  ref_debruijn    := debruijn;
  ref_labels      := labels;
  ref_closure_env := closure_env;
  ref_pretty      := pretty;
  ref_indent      := indent;
  pp_set_margin str_formatter margin;
  pp_set_max_indent str_formatter max_indent

(** Convert terms to strings. *)
let string_of_tm
    ?debruijn ?labels ?closure_env
    ?pretty ?indent ?max_indent ?margin
    ?(prefix = "")
    t =

  setup_print ?debruijn ?labels ?closure_env ?pretty
    ?indent ?max_indent ?margin ();

  fprintf str_formatter "%s" prefix;
  print_tm str_formatter (MATCH, t); flush_str_formatter ()

(* Shorthand for converting values to strings. *)
let string_of_val ?closure_env ?prefix v =
  string_of_tm ?closure_env ?prefix (tm_of_val v)

(* Convert environments to string *)
let string_of_env ?(prefix = "") env =
  setup_print ~closure_env:true ();
  fprintf str_formatter "%s" prefix;
  print_env str_formatter env; flush_str_formatter()
