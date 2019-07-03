(** Functions for converting ast data to strings *)

open Ast
open Pattern
open Format
open Utils

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

(** If the argument is a complete construction of a tuple or record,
    return the constructor and the argument terms. *)
let compl_tr t =
  let rec recurse acc (T{t;_}) = match acc,t with
    | acc,TApp{t1;t2;_} -> recurse (t2::acc) t1
    | [],_ -> None (* Ensures there is at least one argument *)
    | acc,TVal{v=V{v=VRec{pls;rls=[];_} as v;_}}
      when List.length pls = List.length acc -> Some (v,List.rev acc)
    | acc,TVal{v=V{v=VTup{np;_} as v;_}}
      when np = List.length acc -> Some (v,List.rev acc)
    | _ -> None
  in recurse [] t

(** If the argument is a complete construction of a list, return the
    constituent terms. *)
let compl_l t =
  let rec recurse acc (T{t;_}) = match t with
    | TApp{t1=T{t=TApp{t1=T{t=TVal{v=V{v=VCons _;_}};_};
                       t2};_};
           t2=next} ->
      recurse (t2::acc) next
    | TVal{v=V{v=VList{vls=[]};_}} -> Some (List.rev acc)
    | _ -> None
  in recurse [] t

(** Print a term on the given formatter and within the given precedence. *)
let rec print_tm fmt (prec, (T{t=t';_} as t)) =

  let bare_paren t' = match t' with
    | TMatch _                -> prec > MATCH
    | TLam _ | TCont _        -> prec > LAM
    | TIf _                   -> prec > IF
    | TVal{v=V{v=VTup _;_};_} -> prec > TUP
    | TApp _                  -> prec > APP
    | TVar _ | TVal _         -> prec > ATOM
  in

  let pretty_paren t' =
    match compl_tr t with
    | Some (VRec _,_) -> prec > ATOM
    | Some (VTup _,_) -> prec > TUP
    | Some _ -> failwith "Not possible"
    | _ -> match compl_l t with
      | Some _ -> prec > ATOM
      | _ -> match t' with
        | TApp{t1=T{t=TLam{x="_";_};_};_} -> prec > SEMICOLON
        | TApp{t1=T{t=TLam _;_};_}        -> prec > MATCH
        | _ -> bare_paren t'
  in

  let print,paren =
    if !ref_pretty then
      pretty_print,pretty_paren t' else
      bare_print,bare_paren t' in

  if !ref_labels then
    (* TODO This approach to printing labels omits some labels when using
       pretty printing *)
    match t' with
    | TVar _ | TVal _ -> fprintf fmt "%a:%d"   print t (tm_label t)
    | _               -> fprintf fmt "(%a):%d" print t (tm_label t)
  else if paren then
    fprintf fmt "(%a)" print t
  else
    fprintf fmt "%a" print t

(* Function for bare printing (no syntactic sugar) *)
and bare_print fmt (T{t;_}) = match t with

    | TCont{x;t1;_} ->
      fprintf fmt "@[<hov %d>cont %s.@ %a@]" !ref_indent x print_tm (LAM, t1)

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

    | TApp{t1;t2=T{t=TApp _;_} as t2} ->
      fprintf fmt "@[<hv 0>%a@ %a@]" print_tm (APP, t1) print_tm (ATOM, t2)

    | TApp{t1;t2;_} ->
      fprintf fmt "@[<hv 0>%a@ %a@]" print_tm (APP, t1) print_tm (APP, t2)

    | TVar{xat={var_label;_};x;i;_} ->
      let vl = if !ref_debruijn then Printf.sprintf "#%d" i else "" in
      let d = if !ref_labels then Printf.sprintf "|%d" var_label else "" in
      if !ref_labels || !ref_debruijn then
        fprintf fmt "<%s%s%s>" x vl d
      else fprintf fmt "%s" x

    | TVal{v=V{v;_};_} ->
      begin match v with

        | VCont{x;t1;env;_} ->
          fprintf fmt "@[<hov %d>cont%a %s.@ %a@]"
            !ref_indent print_env env x print_tm (LAM, t1)

        | VLam{x;t1;env;_} ->
          fprintf fmt "@[<hov %d>lam%a %s.@ %a@]"
            !ref_indent print_env env x print_tm (LAM, t1)

        | VIf{t1;t2;env;_} ->
          fprintf fmt "@[<hv 0>\
                       @[<hov %d>if%a . then@ %a@]\
                       @ \
                       @[<hov %d>else@ %a@]\
                       @]"
            !ref_indent print_env env
            print_tm (MATCH, t1)
            !ref_indent print_tm (IF, t2)

        | VMatch{cls;env;_} ->
          let inner = List.map (fun (p,t1) ->
              (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
                  (string_of_pat p) print_tm (LAM, t1)))
              cls in
          fprintf fmt "@[<hov %d>match%a@ .@ with@ @[<hv 0>%a@]@]"
            !ref_indent print_env env
            concat (SPACE,inner)

        | VFix   -> fprintf fmt "fix"

        | VUnit   -> fprintf fmt "()"

        | VBool{b;_}      -> fprintf fmt "%B" b
        | VNot            -> fprintf fmt "not"
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
        | VLog        -> fprintf fmt "log"

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

        | VNeg              -> fprintf fmt "neg"

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

        | VDist{d} ->
          begin match d with
            | DNormal{mu=None;   sigma=None;_}    -> fprintf fmt "normal"
            | DNormal{mu=Some f1;sigma=None;_}    -> fprintf fmt "normal(%f)" f1
            | DNormal{mu=Some f1;sigma=Some f2;_} -> fprintf fmt "normal(%f,%f)" f1 f2
            | DNormal _                         -> failwith "Not supported"

            | DUniform{a=None;   b=None;_}    -> fprintf fmt "uniform"
            | DUniform{a=Some f1;b=None;_}    -> fprintf fmt "uniform(%f)" f1
            | DUniform{a=Some f1;b=Some f2;_} -> fprintf fmt "uniform(%f,%f)" f1 f2
            | DUniform _                    -> failwith "Not supported"

            | DGamma{a=None;   b=None;_}    -> fprintf fmt "gamma"
            | DGamma{a=Some f1;b=None;_}    -> fprintf fmt "gamma(%f)" f1
            | DGamma{a=Some f1;b=Some f2;_} -> fprintf fmt "gamma(%f,%f)" f1 f2
            | DGamma _                    -> failwith "Not supported"

            | DExp{lam=None;_}   -> fprintf fmt "exp"
            | DExp{lam=Some f;_} -> fprintf fmt "exp(%f)" f

            | DBern{p=None;_}   -> fprintf fmt "bern"
            | DBern{p=Some f;_} -> fprintf fmt "bern(%f)" f

            | DBeta{a=None;   b=None;_}    -> fprintf fmt "beta"
            | DBeta{a=Some f1;b=None;_}    -> fprintf fmt "beta(%f)" f1
            | DBeta{a=Some f1;b=Some f2;_} -> fprintf fmt "beta(%f,%f)" f1 f2
            | DBeta _                    -> failwith "Not supported"
          end

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
      end

(* Syntactic sugar printing *)
and pretty_print fmt (T{t=t';_} as t) = match compl_tr t with

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
    | None -> match compl_l t with

      (* Lists *)
      | Some args ->
        let inner = List.map (fun t1 ->
            (fun fmt -> fprintf fmt "%a" print_tm (APP, t1))) args in
        fprintf fmt "[@[<hov 0>%a@]]"
          concat (COMMA,inner)

      (* Finally, check for static patterns *)
      | None -> match t' with

        (* Record projection application *)
        | TApp{t1=T{t=TVal{v=V{v=VRecProj{k;_};_}};_};
               t2} ->
          fprintf fmt "%a.%s" print_tm (ATOM, t2) k

        (* Tuple projection applications *)
        | TApp{t1=T{t=TVal{v=V{v=VTupProj{i;_};_}};_};
               t2} ->
          fprintf fmt "%a.%d" print_tm (ATOM, t2) i

        (* If applications *)
        | TApp{t1=T{t=TIf{t1;t2};_};
               t2=t} ->
          fprintf fmt "@[<hv 0>\
                       @[<hov %d>if %a then@ %a@]\
                       @ \
                       @[<hov %d>else@ %a@]\
                       @]"
            !ref_indent print_tm (MATCH, t) print_tm (MATCH, t1)
            !ref_indent print_tm (IF, t2)

        (* Match applications *)
        | TApp{t1=T{t=TMatch{cls};_};t2=t;_} ->
          let inner = List.map (fun (p,t1) ->
              (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" !ref_indent
                  (string_of_pat p) print_tm (LAM, t1)))
              cls in
          fprintf fmt "@[<hov %d>match@ %a@ with@ @[<hv 0>%a@]@]"
            !ref_indent
            print_tm (MATCH, t)
            concat (SPACE,inner)

        (* Sequencing (right associative) *)
        | TApp{t1=T{t=TLam{x="_";t1;_};_};
               t2=T{t=TApp{t1=T{t=TLam{x="_";_};_};_};_} as t2} ->
          fprintf fmt "@[<hv 0>%a;@ %a@]"
            print_tm (IF, t2) print_tm (MATCH, t1)
        | TApp{t1=T{t=TLam{x="_";t1;_};_};t2} ->
          fprintf fmt "@[<hv 0>%a;@ %a@]"
            print_tm (SEMICOLON, t2) print_tm (MATCH, t1)

        (* Let expressions *)
        | TApp{t1=T{t=TLam{x;t1;_};_};t2} ->
          fprintf fmt "@[<hv 0>\
                       @[<hov %d>let %s =@ %a in@]\
                       @ %a@]"
            !ref_indent x print_tm (MATCH, t2) print_tm (MATCH, t1)

        (* Otherwise, fall back to bare printing *)
        | _ -> bare_print fmt t

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
let string_of_val ?closure_env ?prefix ?margin ?max_boxes ?pretty v =
  string_of_tm ?closure_env ?prefix ?margin ?max_boxes ?pretty (tm_of_val v)

(** Convert environments to string *)
let string_of_env ?(prefix = "") env =
  setup_print ~closure_env:true ();
  fprintf str_formatter "%s" prefix;
  print_env str_formatter env; flush_str_formatter()

