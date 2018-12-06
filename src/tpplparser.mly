%{
(* TODO The first special comment of the file is the comment associated with
   the whole module.

   TODO Add anonymous functions, records, tuples, and lists

   TODO Go through where TmVar are used for sample, replace with TmSample *)

open Ast
open Utils

(** Add fix-point, if recursive function *)
let addrec x t =
  let rec hasx t = match t with
    | TmVar(_,y,_) ->  x = y
    | TmLam(_,y,t1) -> if x = y then false else hasx t1
    | TmClos(_,_,_,_) -> failwith "Cannot happen"
    | TmApp(_,t1,t2) -> hasx t1 || hasx t2
    | TmConst(_,_) -> false
    | TmFix _ -> false
    | TmIf(_,_,None) -> false
    | TmIf(_,_,Some(t1)) -> hasx t1

    | TmRec _ -> false
    | TmProj _ -> false

    | TmUtest(_,t1,t2) -> hasx t1 || hasx t2

    | TmList _ -> failwith "TODO"
    | TmConcat _ -> failwith "TODO"

    | TmInfer _ -> failwith "TODO"
    | TmLogPdf _ -> failwith "TODO"
    | TmSample _ -> failwith "TODO"
    | TmWeight _ -> failwith "TODO"
    | TmDWeight _ -> failwith "TODO"
  in if hasx t then
    TmApp(na,TmFix(na),TmLam(na,x,t))
  else t

%}

/* Misc */
%token EOF
%token <string> IDENT
%token <string> FUNIDENT

/* Keywords */
%token FUNC
%token IF
%token THEN
%token ELSE
%token UTEST
%token OBSERVE

/* Literals */
%token TRUE
%token FALSE
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <char> CHAR

/* Symbolic tokens */
%token TILDE         /* "~"  */
%token LPAREN        /* "("  */
%token RPAREN        /* ")"  */
%token LCURLY        /* "{"  */
%token RCURLY        /* "}"  */
%token COLON         /* ":"  */
%token COMMA         /* ","  */
%token DOT           /* "."  */

%token EQ            /* "="  */
%token ADD           /* "+"  */
%token SUB           /* "-"  */
%token MUL           /* "*"  */
%token DIV           /* "/"  */
%token MOD           /* "%"  */
%token LESS          /* "<"  */
%token LESSEQUAL     /* "<=" */
%token GREAT         /* ">"  */
%token GREATEQUAL    /* ">=" */
%token SHIFTLL       /* "<<" */
%token SHIFTRL       /* ">>" */
%token SHIFTRA       /* ">>>" */
%token EQUAL         /* "==" */
%token NOTEQUAL      /* "!=" */
%token NOT           /* "!"   */
%token OR            /* "||" */
%token AND           /* "&&" */

%start main

%nonassoc IF
%left OR
%left AND
%left LESS LESSEQUAL GREAT GREATEQUAL EQUAL NOTEQUAL
%left SHIFTLL SHIFTRL SHIFTRA
%nonassoc NOT LOG
%left ADD SUB
%left MUL DIV MOD
%nonassoc USUB
%left DOT

%type <Ast.tm> main

%%

main:
  | treeppl_scope EOF { $1 }

/* ************************** TREEPPL ********************************* */

/*
 * Outermost treeppl_scope. As an example, this enables writing
 * { -1 } instead of { (-1) }.
 */
treeppl_scope:
  | expr sep_treeppl_scope
      { match $2 with
        | TmConst(_,CUnit) -> $1
        | _ -> TmApp(na,TmLam(na,"_",$2),$1) }
  | treeppl_scope_aux { $1 }

sep_treeppl_scope:
  | sep_expr sep_treeppl_scope
      { match $2 with
        | TmConst(_,CUnit) -> $1
        | _ -> TmApp(na,TmLam(na,"_",$2),$1) }
  | treeppl_scope_aux { $1 }

treeppl_scope_aux:
  | { TmConst(na,CUnit) }
  | FUNC FUNIDENT params RPAREN expr sep_treeppl_scope
      { let rec mkfun lst =
          (match lst with
          | x::xs -> TmLam(na,x,mkfun xs)
          | [] -> $5 ) in
        let f = if List.length $3 = 0 then ["_"] else $3
        in TmApp(na,TmLam(na,$2,$6), addrec $2 (mkfun f)) }
  | IDENT EQ expr sep_treeppl_scope
      { TmApp(na,TmLam(na,$1,$4),$3) }
  | IDENT TILDE expr sep_treeppl_scope
      { let sample = TmVar(na,"sample",noidx) in
        TmApp(na,TmLam(na,$1,$4),TmApp(na,sample,$3)) }
  | OBSERVE expr TILDE expr sep_treeppl_scope
      { let logpdf = TmVar(na,"logpdf",noidx) in
        let v = $2 in
        let inner = TmApp(na,TmApp(na,logpdf,v),$4) in
        let weight = TmVar(na,"weight",noidx) in
        let outer = TmApp(na,weight,inner) in
        TmApp(na,TmLam(na,"_",$5),outer) }
  | UTEST expr sep_expr sep_treeppl_scope
      { let a = { na with pos = Parsing.symbol_start_pos () } in
        TmApp(na,TmLam(na,"_",$4),TmUtest(a,$2,$3)) }

expr:
  | SUB expr %prec USUB  { TmApp(na,TmConst(na,CNeg),$2) }
  | expr ADD expr        { TmApp(na,TmApp(na,TmConst(na,CAdd(None)),$1),$3) }
  | expr SUB expr        { TmApp(na,TmApp(na,TmConst(na,CSub(None)),$1),$3) }
  | expr MUL expr        { TmApp(na,TmApp(na,TmConst(na,CMul(None)),$1),$3) }
  | expr DIV expr        { TmApp(na,TmApp(na,TmConst(na,CDiv(None)),$1),$3) }
  | expr MOD expr        { TmApp(na,TmApp(na,TmConst(na,CMod(None)),$1),$3) }
  | expr LESS expr       { TmApp(na,TmApp(na,TmConst(na,CLt(None)),$1),$3) }
  | expr LESSEQUAL expr  { TmApp(na,TmApp(na,TmConst(na,CLeq(None)),$1),$3) }
  | expr GREAT expr      { TmApp(na,TmApp(na,TmConst(na,CGt(None)),$1),$3)}
  | expr GREATEQUAL expr { TmApp(na,TmApp(na,TmConst(na,CGeq(None)),$1),$3) }
  | expr EQUAL expr      { TmApp(na,TmApp(na,TmConst(na,CEq(None)),$1),$3) }
  | expr NOTEQUAL expr   { TmApp(na,TmApp(na,TmConst(na,CNeq(None)),$1),$3) }
  | expr SHIFTLL expr    { TmApp(na,TmApp(na,TmConst(na,CSll(None)),$1),$3) }
  | expr SHIFTRL expr    { TmApp(na,TmApp(na,TmConst(na,CSrl(None)),$1),$3) }
  | expr SHIFTRA expr    { TmApp(na,TmApp(na,TmConst(na,CSra(None)),$1),$3) }
  | expr AND expr        { TmApp(na,TmApp(na,TmConst(na,CAnd(None)),$1),$3) }
  | expr OR expr         { TmApp(na,TmApp(na,TmConst(na,COr(None)),$1),$3) }
  | expr DOT IDENT %prec DOT { TmProj(na,$1,$3) }
  | expr_aux { $1 }

/*
 * Separable expressions (expressions not starting with a minus sign) for
 * sequencing. Combined with expr, enables function calls such as
 * foo(-1, -2) instead of foo((-1), (-2)).
 * Also gives other nice properties such as being able to write
 * x = -1 instead of x = (-1). Gives a difference between writing
 * -1 -1 (evaluates to -2) and
 * -1 (-1) (evaluates to -1 because of sequencing)
 * Unfortunate with code duplication. TODO Is there a better way?
 */
sep_expr:
  | sep_expr ADD expr { TmApp(na,TmApp(na,TmConst(na,CAdd(None)),$1),$3) }
  | sep_expr SUB expr { TmApp(na,TmApp(na,TmConst(na,CSub(None)),$1),$3) }
  | sep_expr MUL expr { TmApp(na,TmApp(na,TmConst(na,CMul(None)),$1),$3) }
  | sep_expr DIV expr { TmApp(na,TmApp(na,TmConst(na,CDiv(None)),$1),$3) }
  | sep_expr MOD expr { TmApp(na,TmApp(na,TmConst(na,CMod(None)),$1),$3) }
  | sep_expr LESS expr { TmApp(na,TmApp(na,TmConst(na,CLt(None)),$1),$3) }
  | sep_expr LESSEQUAL expr
      { TmApp(na,TmApp(na,TmConst(na,CLeq(None)),$1),$3) }
  | sep_expr GREAT expr { TmApp(na,TmApp(na,TmConst(na,CGt(None)),$1),$3)}
  | sep_expr GREATEQUAL expr
      { TmApp(na,TmApp(na,TmConst(na,CGeq(None)),$1),$3) }
  | sep_expr EQUAL expr { TmApp(na,TmApp(na,TmConst(na,CEq(None)),$1),$3) }
  | sep_expr NOTEQUAL expr
      { TmApp(na,TmApp(na,TmConst(na,CNeq(None)),$1),$3) }
  | sep_expr SHIFTLL expr { TmApp(na,TmApp(na,TmConst(na,CSll(None)),$1),$3) }
  | sep_expr SHIFTRL expr { TmApp(na,TmApp(na,TmConst(na,CSrl(None)),$1),$3) }
  | sep_expr SHIFTRA expr { TmApp(na,TmApp(na,TmConst(na,CSra(None)),$1),$3) }
  | sep_expr DOT IDENT %prec DOT { TmProj(na,$1,$3) }
  | sep_expr AND expr { TmApp(na,TmApp(na,TmConst(na,CAnd(None)),$1),$3) }
  | sep_expr OR expr { TmApp(na,TmApp(na,TmConst(na,COr(None)),$1),$3) }
  | expr_aux { $1 }

expr_aux:
  | FUNIDENT exprs RPAREN
      { let rec mkapps lst =
          match lst with
          | t::ts ->  TmApp(na,mkapps ts,t)
          | [] -> TmVar(na,$1,noidx)
        in mkapps
          (if List.length $2 = 0 then [nop] else (List.rev $2)) }
  | IF expr THEN expr ELSE expr %prec IF
      { TmApp(na,
            TmApp(na,
              TmApp(na,TmIf(na,None,None),$2),
              TmLam(na,"",$4)),
            TmLam(na,"",$6)) }
  | LPAREN expr RPAREN   { $2 }
  | LCURLY treeppl_scope RCURLY  { $2 }
  | LCURLY record RCURLY { TmRec(na,$2) }
  | NOT expr   { TmApp(na,TmConst(na,CNot),$2) }
  | IDENT      { TmVar(na,$1,noidx) }
  | CHAR       { TmConst(na, CChar($1)) }
  | STRING     { TmConst(na, CString($1)) }
  | INT        { TmConst(na, CInt($1)) }
  | FLOAT      { TmConst(na, CFloat($1)) }
  | TRUE       { TmConst(na, CBool(true)) }
  | FALSE      { TmConst(na, CBool(false)) }

record:
  | IDENT COLON expr { StrMap.singleton $1 $3 }
  | IDENT COLON expr COMMA record { StrMap.add $1 $3 $5 }

params:
  | { [] }
  | paramlist { $1 }

paramlist:
  | IDENT { [$1] }
  | IDENT COMMA paramlist { $1 :: $3 }

exprs:
  | { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }


