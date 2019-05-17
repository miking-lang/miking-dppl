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

(** Sets up the formatter. *)
let setup_print
    ?(debruijn       = false)
    ?(labels         = false)
    ?(closure_env    = false)
    ?(pretty         = true)
    ?(indent         = 2)
    ?(max_indent     = 68)
    ?(margin         = 80)
    ?(max_boxes      = max_int)
    () =
  ref_debruijn    := debruijn;
  ref_labels      := labels;
  ref_closure_env := closure_env;
  ref_pretty      := pretty;
  ref_indent      := indent;
  pp_set_margin str_formatter margin;
  pp_set_max_indent str_formatter max_indent;
  pp_set_max_boxes str_formatter max_boxes

(** Simple enum used in the concat function in string_of_tm *)
type sep =
  | SPACE
  | COMMA

(** Function for concatenating a list of fprintf calls using a given
     separator. *)
let rec concat fmt (sep, ls) = match ls with
  | [] -> ()
  | [f] -> f fmt
  | f :: ls -> match sep with
    | SPACE -> fprintf fmt "%t@ %a" f concat (sep, ls)
    | COMMA -> fprintf fmt "%t,@,%a" f concat (sep, ls)

(** If the argument is a complete construction of a record or tuple,
    return the constructor and the argument terms. *)
let compl_tr tm =
  let rec recurse acc tm = match acc,tm with
    | acc,TApp{t1;t2;_} -> recurse (t2::acc) t1
    | [],_ -> None (* Ensures there is at least one argument *)
    | acc,TVal{v=VRec{pls;rls=[];_} as v;_}
      when List.length pls = List.length acc -> Some (v,List.rev acc)
    | acc,TVal{v=VTup{np;_} as v;_}
      when np = List.length acc -> Some (v,List.rev acc)
    | _ -> None
  in recurse [] tm

(** If the argument is a complete construction of a list, return the
    constituent terms. *)
let compl_l tm =
  let rec recurse acc t = match t with
    | TApp{t1=TApp{t1=TVal{v=VCons _;_};t2;_};t2=next;_} ->
      recurse (t2::acc) next
    | TVal{v=VList{vls=[];_};_} -> Some (List.rev acc)
    | _ -> None
  in recurse [] tm

(** Print a term on the given formatter and within the given precedence. *)
let rec print_tm fmt (prec, t) =

  (* Function for bare printing (no syntactic sugar) *)
  let bare fmt t = match t with

    | TLam{x;t1;_} ->
      fprintf fmt "@[<hov %d>lam %s.@ %a@]" !ref_indent x print_tm (LAM, t1)

    | TIf{t1;t2;_} ->
      fprintf fmt "@[<hv 0>\
                   @[<hov %d>if . then@ %a@]\
                   @ \
                   @[<hov %d>else@ %a@]\
                   @]"
        !ref_indent print_tm (MATCH, t1)
        !ref_indent print_tm (IF, t2)

    | TMatch{cls;_} ->
      let inner = List.map (fun (p,t1) ->
          (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
              (string_of_pat p) print_tm (LAM, t1)))
          cls in
      fprintf fmt "@[<hov %d>match@ .@ with@ @[<hv 0>%a@]@]"
        !ref_indent
        concat (SPACE,inner)

    | TApp{t1;t2=TApp _ as t2;_} ->
      fprintf fmt "@[<hv 0>%a@ %a@]" print_tm (APP, t1) print_tm (ATOM, t2)

    | TApp{t1;t2;_} ->
      fprintf fmt "@[<hv 0>%a@ %a@]" print_tm (APP, t1) print_tm (APP, t2)

    | TVar{vat={var_label;_};x;i;_} ->
      let vl = if !ref_debruijn then Printf.sprintf "#%d" i else "" in
      let d = if !ref_labels then Printf.sprintf "|%d" var_label else "" in
      if !ref_labels || !ref_debruijn then
        fprintf fmt "<%s%s%s>" x vl d
      else fprintf fmt "%s" x

    | TVal{v;_} -> match v with

      | VClos{x;t1;env;_} ->
        fprintf fmt "@[<hov %d>clos%a %s.@ %a@]"
          !ref_indent print_env env x print_tm (LAM, t1)

      | VClosIf{t1;t2;env;_} ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>if%a . then@ %a@]\
                     @ \
                     @[<hov %d>else@ %a@]\
                     @]"
          !ref_indent print_env env
          print_tm (MATCH, t1)
          !ref_indent print_tm (IF, t2)

      | VClosMatch{cls;env;_} ->
        let inner = List.map (fun (p,t1) ->
            (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
                (string_of_pat p) print_tm (LAM, t1)))
            cls in
        fprintf fmt "@[<hov %d>match%a@ .@ with@ @[<hv 0>%a@]@]"
          !ref_indent print_env env
          concat (SPACE,inner)

      | VFix _ -> fprintf fmt "fix"

      | VUnit _ -> fprintf fmt "()"

      | VBool{b;_}      -> fprintf fmt "%B" b
      | VNot _          -> fprintf fmt "not"
      | VAnd{b1=None;_}    -> fprintf fmt "and"
      | VAnd{b1=Some(v);_} -> fprintf fmt "and(%B)" v
      | VOr{b1=None;_}     -> fprintf fmt "or"
      | VOr{b1=Some(v);_}  -> fprintf fmt "or(%B)" v

      | VChar{c;_}      -> fprintf fmt "%C" c

      | VString{s;_}    -> fprintf fmt "%S" s

      | VInt{i;_}       -> fprintf fmt "%d" i
      | VMod{i1=None;_}    -> fprintf fmt "mod"
      | VMod{i1=Some(v);_} -> fprintf fmt "mod(%d)" v
      | VSll{i1=None;_}    -> fprintf fmt "sll"
      | VSll{i1=Some(v);_} -> fprintf fmt "sll(%d)" v
      | VSrl{i1=None;_}    -> fprintf fmt "srl"
      | VSrl{i1=Some(v);_} -> fprintf fmt "srl(%d)" v
      | VSra{i1=None;_}    -> fprintf fmt "sra"
      | VSra{i1=Some(v);_} -> fprintf fmt "sra(%d)" v

      | VFloat{f;_} -> fprintf fmt "%f" f
      | VLog _      -> fprintf fmt "log"

      | VAdd{v1=Some v;_} -> fprintf fmt "add(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VAdd{v1=None;_}   -> fprintf fmt "add"

      | VSub{v1=Some v;_} -> fprintf fmt "sub(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VSub{v1=None;_}   -> fprintf fmt "sub"

      | VMul{v1=Some v;_} -> fprintf fmt "mul(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VMul{v1=None;_}   -> fprintf fmt "mul"

      | VDiv{v1=Some v;_} -> fprintf fmt "div(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VDiv{v1=None;_}   -> fprintf fmt "div"

      | VNeg _            -> fprintf fmt "neg"

      | VLt{v1=Some v;_}  -> fprintf fmt "lt(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VLt{v1=None;_}    -> fprintf fmt "lt"

      | VLeq{v1=Some v;_} -> fprintf fmt "leq(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VLeq{v1=None;_}   -> fprintf fmt "leq"

      | VGt{v1=Some v;_}  -> fprintf fmt "gt(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VGt{v1=None;_}    -> fprintf fmt "gt"

      | VGeq{v1=Some v;_} -> fprintf fmt "geq(%a)"
                               print_tm (MATCH, tm_of_val v)
      | VGeq{v1=None;_}   -> fprintf fmt "geq"

      | VEq{v1=None;_}   -> fprintf fmt "eq"
      | VEq{v1=Some v;_} -> fprintf fmt "eq(%a)" print_tm (MATCH, tm_of_val v)

      | VNeq{v1=None;_}   -> fprintf fmt "neq"
      | VNeq{v1=Some v;_} -> fprintf fmt "neq(%a)"
                               print_tm (MATCH, tm_of_val v)

      | VNormal{mu=None;   sigma=None;_}    -> fprintf fmt "normal"
      | VNormal{mu=Some f1;sigma=None;_}    -> fprintf fmt "normal(%f)" f1
      | VNormal{mu=Some f1;sigma=Some f2;_} -> fprintf fmt "normal(%f,%f)"
                                                 f1 f2
      | VNormal _                           -> failwith "Not supported"

      | VUniform{a=None;   b=None;_}    -> fprintf fmt "uniform"
      | VUniform{a=Some f1;b=None;_}    -> fprintf fmt "uniform(%f)" f1
      | VUniform{a=Some f1;b=Some f2;_} -> fprintf fmt "uniform(%f,%f)" f1 f2
      | VUniform _                      -> failwith "Not supported"

      | VGamma{a=None;   b=None;_}    -> fprintf fmt "gamma"
      | VGamma{a=Some f1;b=None;_}    -> fprintf fmt "gamma(%f)" f1
      | VGamma{a=Some f1;b=Some f2;_} -> fprintf fmt "gamma(%f,%f)" f1 f2
      | VGamma _                      -> failwith "Not supported"

      | VExp{lam=None;_}   -> fprintf fmt "exp"
      | VExp{lam=Some f;_} -> fprintf fmt "exp(%f)" f

      | VBern{p=None;_}   -> fprintf fmt "bern"
      | VBern{p=Some f;_} -> fprintf fmt "bern(%f)" f

      | VLogPdf{v1=None;_}   -> fprintf fmt "logpdf"
      | VLogPdf{v1=Some v;_} -> fprintf fmt "logpdf(%a)"
                                  print_tm (MATCH,tm_of_val v)

      | VSample _ -> fprintf fmt "sample"

      | VTup{np;varr;_} ->
        let inner = Array.map (fun v ->
            (fun fmt -> fprintf fmt "%a" print_tm (APP, tm_of_val v)))
            varr in
        let inner =
          replicate np (fun fmt -> fprintf fmt "") @ Array.to_list inner in
        fprintf fmt "@[<hov 0>%a@]" concat (COMMA,inner)

      | VRec{pls;rls;_} ->
        let inner = List.map (fun (k, v) ->
            (fun fmt ->
               fprintf fmt "%s:%a" k print_tm (APP, tm_of_val v))) rls in

        let inner =
          List.map (fun k -> (fun fmt -> fprintf fmt "%s:" k)) (List.rev pls)
          @ inner in
        fprintf fmt "{@[<hov 0>%a@]}" concat (COMMA,inner)

      | VList{vls;_} ->
        let inner = List.map (fun v ->
            (fun fmt ->
               fprintf fmt "%a" print_tm (APP, tm_of_val v))) vls in
        fprintf fmt "[@[<hov 0>%a@]]"
          concat (COMMA,inner)

      | VCons{v1=None;_}   -> fprintf fmt "cons"
      | VCons{v1=Some v;_} -> fprintf fmt "cons(%a)"
                                print_tm (MATCH, tm_of_val v)

      | VRecProj{k;_} -> fprintf fmt "(.).%s" k
      | VTupProj{i;_} -> fprintf fmt "(.).%d" i

      | VUtest{v1=Some v;_} -> fprintf fmt "utest(%a)"
                                 print_tm (MATCH, tm_of_val v)
      | VUtest _            -> fprintf fmt "utest"

      | VConcat{v1=None;_}   -> fprintf fmt "concat"
      | VConcat{v1=Some v;_} -> fprintf fmt "concat(%a)"
                                  print_tm (MATCH, tm_of_val v)

      | VWeight _ -> fprintf fmt "weight"

      | VResamp{dyn;cont=None;_} ->
        fprintf fmt "%sresample" (if dyn then "dyn" else "")
      | VResamp{dyn;cont=Some v;_} ->
        fprintf fmt "%sresample(%a)" (if dyn then "dyn" else "")
          print_tm (MATCH, tm_of_val v)

  in

  (* If pretty printing, try to find common dynamic-sized constructs
     (tuples, records, and lists) *)
  let trmatch = if !ref_pretty then compl_tr t else None in
  let lmatch = if !ref_pretty then compl_l t else None in

  (* Syntactic sugar printing *)
  let sugar fmt t = match trmatch with

    (* Records *)
    | Some (VRec{pls;_},args) ->
      let inner = List.map (fun (k, t1) ->
          (fun fmt -> fprintf fmt "%s:%a" k print_tm (APP, t1)))
          (List.combine (List.rev pls) args) in
      fprintf fmt "{@[<hov 0>%a@]}"
        concat (COMMA,inner)

    (* Tuples *)
    | Some (VTup _,args) ->
      let inner = List.map (fun t1 ->
          (fun fmt -> fprintf fmt "%a" print_tm (APP, t1))) args in
      fprintf fmt "@[<hov 0>%a@]"
        concat (COMMA,inner)


    | Some _ -> failwith "Not possible"

    (* Not a complete tuple or record construction. Check if it is a complete
       list. *)
    | None -> match lmatch with

      (* Lists *)
      | Some args ->
        let inner = List.map (fun t1 ->
            (fun fmt -> fprintf fmt "%a" print_tm (APP, t1))) args in
        fprintf fmt "[@[<hov 0>%a@]]"
          concat (COMMA,inner)

      (* Finally, check for static patterns *)
      | None -> match t with

        (* Record projection application *)
        | TApp{t1=TVal{v=VRecProj{k;_};_};t2;_} ->
          fprintf fmt "%a.%s" print_tm (ATOM, t2) k

        (* Tuple projection applications *)
        | TApp{t1=TVal{v=VTupProj{i;_};_};t2;_} ->
          fprintf fmt "%a.%d" print_tm (ATOM, t2) i

        (* If applications *)
        | TApp{t1=TIf{t1;t2;_};t2=t;_} ->
          fprintf fmt "@[<hv 0>\
                       @[<hov %d>if %a then@ %a@]\
                       @ \
                       @[<hov %d>else@ %a@]\
                       @]"
            !ref_indent print_tm (MATCH, t) print_tm (MATCH, t1)
            !ref_indent print_tm (IF, t2)

        (* Match applications *)
        | TApp{t1=TMatch{cls;_};t2=t;_} ->
          let inner = List.map (fun (p,t1) ->
              (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
                  (string_of_pat p) print_tm (LAM, t1)))
              cls in
          fprintf fmt "@[<hov %d>match@ %a@ with@ @[<hv 0>%a@]@]"
            !ref_indent
            print_tm (MATCH, t)
            concat (SPACE,inner)

        (* Sequencing (right associative) *)
        | TApp{t1=TLam{x="_";t1;_};
               t2=TApp{t1=TLam{x="_";_};_} as t2;_} ->
          fprintf fmt "@[<hv 0>%a;@ %a@]"
            print_tm (IF, t2) print_tm (MATCH, t1)
        | TApp{t1=TLam{x="_";t1;_};t2;_} ->
          fprintf fmt "@[<hv 0>%a;@ %a@]"
            print_tm (SEMICOLON, t2) print_tm (MATCH, t1)

        (* Let expressions *)
        | TApp{t1=TLam{x;t1;_};t2;_} ->
          fprintf fmt "@[<hv 0>\
                       @[<hov %d>let %s =@ %a in@]\
                       @ %a@]"
            !ref_indent x print_tm (MATCH, t2) print_tm (MATCH, t1)

        (* Otherwise, fall back to bare printing *)
        | _ -> bare fmt t

  in

  (* Check if this term should be parenthesized *)
  let paren = match trmatch with
    | Some (VRec _,_) -> prec > ATOM
    | Some (VTup _,_) -> prec > TUP
    | Some _ -> failwith "Not possible"
    | _ -> match lmatch with
      | Some _ -> prec > ATOM
      | _ -> match t with
        | TApp{t1=TLam{x="_";_};_} when !ref_pretty -> prec > SEMICOLON
        | TApp{t1=TLam _;_}        when !ref_pretty -> prec > LAM

        | TMatch _         -> prec > MATCH
        | TLam _           -> prec > LAM
        | TIf _            -> prec > IF
        | TVal{v=VTup _;_} -> prec > TUP
        | TApp _           -> prec > APP
        | TVar _ | TVal _  -> prec > ATOM
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

(** Print an environment on the given formatter. *)
and print_env fmt env =
  if !ref_closure_env then
    let inner = List.mapi (fun i t ->
        (fun fmt ->
           fprintf fmt "%d -> %a" i print_tm (MATCH, t))) env in
    fprintf fmt "[@[<hov 0>%a@]]" concat (COMMA,inner)
  else
    fprintf fmt ""

(** Convert terms to strings.
    TODO Messy with optional arguments passing. Alternatives? *)
let string_of_tm
    ?debruijn ?labels ?closure_env ?pretty ?indent ?max_indent ?margin
    ?max_boxes ?(prefix = "")
    t =

  setup_print ?debruijn ?labels ?closure_env ?pretty ?indent ?max_indent
    ?margin ?max_boxes ();

  fprintf str_formatter "%s" prefix;
  print_tm str_formatter (MATCH, t); flush_str_formatter ()

(** Shorthand for converting values to strings. *)
let string_of_val ?closure_env ?prefix ?margin ?max_boxes v =
  string_of_tm ?closure_env ?prefix ?margin ?max_boxes (tm_of_val v)

(** Convert environments to string *)
let string_of_env ?(prefix = "") env =
  setup_print ~closure_env:true ();
  fprintf str_formatter "%s" prefix;
  print_env str_formatter env; flush_str_formatter()
