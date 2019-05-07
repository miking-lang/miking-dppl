(** Pretty printing of terms based on the Format module of the OCaml standard
    library *)

open Ast
open Pattern
open Format
open Const

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

(** Convert terms to strings. TODO Labels *)
let string_of_tm
    ?(debruijn   = false)
    ?(labels     = false)
    ?(pretty     = true)
    ?(indent     = 2)
    ?(max_indent = 68)
    ?(margin     = 80)
    t =

  (* Set right margin and maximum indentation *)
  pp_set_margin str_formatter margin;
  pp_set_max_indent str_formatter max_indent;

  (* Function for concatenating a list of fprintf calls using a given
     separator. *)
  let rec concat fmt (sep, ls) = match ls with
    | [] -> ()
    | [f] -> f fmt
    | f :: ls -> match sep with
      | SPACE -> fprintf fmt "%t@ %a" f concat (sep, ls)
      | COMMA -> fprintf fmt "%t,@,%a" f concat (sep, ls)
  in

  let rec recurse fmt (prec, t) =

    (* Function for bare printing (no syntactic sugar) *)
    let bare fmt t = match t with

      | TmMatch(_,t,cases) ->
        let inner = List.map (fun (p,t1) ->
            (fun fmt -> fprintf fmt "@[<hov %d>| %s ->@ %a@]" indent
                (string_of_pat p) recurse (LAM, t1)))
            cases in
        fprintf fmt "@[<hov %d>match@ %a@ with@ @[<hv 0>%a@]@]"
          indent
          recurse (MATCH, t)
          concat (SPACE,inner)

      | TmLam(_,x,t1) ->
        fprintf fmt "@[<hov %d>lam %s.@ %a@]" indent x recurse (MATCH, t1)

      (* TODO Also print closure environment if an argument is given *)
      | TmClos(_,x,t1,_) ->
        fprintf fmt "@[<hov %d>clos %s.@ %a@]" indent x recurse (MATCH, t1)

      | TmIf(_,t1,t11,t12) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>if %a then@ %a@]\
                     @ \
                     @[<hov %d>else@ %a@]\
                     @]"
          indent recurse (MATCH, t1) recurse (MATCH, t11)
          indent recurse (MATCH, t12)

      | TmTup(_,tarr) ->
        let inner = Array.map (fun t1 ->
            (fun fmt -> fprintf fmt "%a" recurse (APP, t1))) tarr in
        fprintf fmt "@[<hov 0>%a@]"
          concat (COMMA,Array.to_list inner)

      | TmRec(_,sm) ->
        let inner = List.map (fun (k, t1) ->
           (fun fmt -> fprintf fmt "%s:%a" k recurse (MATCH, t1))) sm in
        fprintf fmt "{@[<hov 0>%a@]}"
          concat (COMMA,inner)

      | TmList(_,ls) ->
        let inner = List.map (fun t1 ->
           (fun fmt -> fprintf fmt "%a" recurse (MATCH, t1))) ls in
        fprintf fmt "[@[<hov 0>%a@]]"
          concat (COMMA,inner)

      (* TODO Make applications look nicer *)
      | TmApp(_,t1,(TmApp _ as t2)) ->
        fprintf fmt "@[<hv 0>%a@ %a@]" recurse (APP, t1) recurse (ATOM, t2)

      | TmApp(_,t1,t2) ->
        fprintf fmt "@[<hv 0>%a@ %a@]" recurse (APP, t1) recurse (APP, t2)

      | TmVar({var_label;_},x,n) ->
        let vl = if debruijn then Printf.sprintf ":%d" n else "" in
        let d = if labels then Printf.sprintf ":%d" var_label else "" in
        if labels || debruijn then
          fprintf fmt "<%s%s%s>" x vl d
        else fprintf fmt "%s" x

      | TmRecProj(_,t1,x)   -> fprintf fmt "%a.%s" recurse (APP, t1) x
      | TmTupProj(_,t1,i)   -> fprintf fmt "%a.%d" recurse (APP, t1) i

      | TmConst(_,c)        -> fprintf fmt "%s" (string_of_const c)

      | TmFix _             -> fprintf fmt "fix"

      | TmUtest(_,Some t1)  -> fprintf fmt "utest(%a)" recurse (MATCH, t1)
      | TmUtest _           -> fprintf fmt "utest"

      | TmConcat(_,None)    -> fprintf fmt "concat"
      | TmConcat(_,Some t1) -> fprintf fmt "concat(%a)" recurse (MATCH, t1)

      | TmLogPdf(_,None)    -> fprintf fmt "logpdf"
      | TmLogPdf(_,Some t1) -> fprintf fmt "logpdf(%a)" recurse (MATCH, t1)

      | TmSample _          -> fprintf fmt "sample"

      | TmWeight _          -> fprintf fmt "weight"

      | TmResamp(_,None,None)       -> fprintf fmt "resample"
      | TmResamp(_,Some t1,None)    -> fprintf fmt "resample(%a)"
                                         recurse (MATCH, t1)
      | TmResamp(_,Some t1,Some c1) -> fprintf fmt "resample(%a,%s)"
                                         recurse (APP, t1)
                                         (string_of_const c1)
      | TmResamp _                  -> failwith "Invalid TmResamp"

    in

    (* Syntactic sugar printing *)
    let sugar fmt t = match t with

      (* Sequencing (right associative) *)
      | TmApp(_,TmLam(_,"_",t2),
              (TmApp(_,TmLam(_,"_",_),_) as t1)) ->
        fprintf fmt "@[<hv 0>%a;@ %a@]"
          recurse (IF, t1) recurse (MATCH, t2)
      | TmApp(_,TmLam(_,"_",t2),t1) ->
        fprintf fmt "@[<hv 0>%a;@ %a@]"
          recurse (SEMICOLON, t1) recurse (MATCH, t2)

      (* Let expressions *)
      | TmApp(_,TmLam(_,x,t1),t2) ->
        fprintf fmt "@[<hv 0>\
                     @[<hov %d>let %s =@ %a in@]\
                     @ %a@]"
          indent x recurse (MATCH, t2) recurse (MATCH, t1)

      | _ -> bare fmt t
    in

    (* Check if this term should be parenthesized *)
    let paren = match t with

      | TmApp(_,TmLam(_,"_",_),_) when pretty -> prec > SEMICOLON
      | TmApp(_,TmLam(_,_,_),_) when pretty   -> prec > LAM

      | TmMatch _          -> prec > MATCH
      | TmLam _ | TmClos _ -> prec > LAM
      | TmIf _             -> prec > IF
      | TmTup _            -> prec > TUP
      | TmApp _            -> prec > APP
      | TmVar _     | TmConst _
      | TmFix _     | TmRec _     | TmTupProj _
      | TmRecProj _ | TmList _    | TmLogPdf _
      | TmSample _  | TmUtest _   | TmWeight _
      | TmConcat _  | TmResamp _ -> prec > ATOM

    in

    let p = if pretty then sugar else bare in
    if labels then
      match t with
      | TmResamp _ | TmVar _  | TmConst _
      | TmWeight _ | TmFix _  | TmRec _
      | TmTupProj _ | TmRecProj _ | TmList _
      | TmLogPdf _ | TmSample _  | TmUtest _
      | TmConcat _ -> fprintf fmt "%a:%d"   p t (tm_label t)
      | _          -> fprintf fmt "(%a):%d" p t (tm_label t)
    else if paren then
      fprintf fmt "(%a)" p t
    else fprintf fmt "%a" p t

  in recurse str_formatter (MATCH, t); flush_str_formatter ()

(** Convert environments to strings TODO Merge with TmClos in string_of_tm *)
let string_of_env env =
  "[" ^ (List.mapi (fun i t -> Printf.sprintf " %d -> " i ^ string_of_tm t) env
            |> String.concat ",") ^ "]"

