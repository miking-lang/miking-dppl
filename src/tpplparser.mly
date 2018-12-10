%{
(* TODO The first special comment of the file is the comment associated with
   the whole module. *)

open Ast

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

    | TmUtest(_,t1,t2) -> hasx t1 || hasx t2

    | TmMatch(_,t1,pls) -> hasx t1 || List.exists (fun (_,te) -> hasx te) pls

    | TmRec(_,rels) -> List.exists (fun (_,te) -> hasx te) rels
    | TmRecProj(_,t1,_) -> hasx t1

    | TmTup(_,tarr) -> Array.exists hasx tarr
    | TmTupProj(_,t1,_) -> hasx t1

    | TmList(_,tls) -> List.exists hasx tls

    | TmConcat _ -> false

    | TmInfer _ -> false
    | TmLogPdf _ -> false
    | TmSample _ -> false
    | TmWeight _ -> false
    | TmDWeight _ -> false
  in if hasx t then
    TmApp(na,TmFix(na),TmLam(na,x,t))
  else t

let rec mkfun params body = match params with
  | x::xs -> TmLam(na,x,mkfun xs body)
  | [] -> body

let rec mkapps args func = match args with
  | t::ts -> TmApp(na,mkapps ts func,t)
  | [] -> TmVar(na,func,noidx)

%}

/* Misc */
%token EOF
%token <string> IDENT
%token <string> FUNIDENT

/* Keywords */
%token FUNC
%token LAM
%token IF
%token THEN
%token ELSE
%token UTEST
%token OBSERVE
%token MATCH
%token WITH

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
%token LSQUARE       /* "["  */
%token RSQUARE       /* "]"  */
%token COLON         /* ":"  */
%token DCOLON        /* "::"  */
%token SEMICOLON     /* ";"  */
%token COMMA         /* ","  */
%token DOT           /* "."  */
%token VBAR          /* "|"  */
%token RARROW        /* "->"  */

/* Operators */
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
%token CONCAT        /* "++" */

%start main


%nonassoc LOW
%nonassoc VBAR

%left OR
%left AND
%left LESS LESSEQUAL GREAT GREATEQUAL EQUAL NOTEQUAL
%left SHIFTLL SHIFTRL SHIFTRA
%left CONCAT
%right DCOLON
%left ADD SUB
%left MUL DIV MOD
%nonassoc NOT USUB
%nonassoc DOT

%type <Ast.tm> main

%%

main:
  | seq EOF { $1 }
  | EOF { nop }

/* ************************** TREEPPL ********************************* */

seq:
  | texpr { $1 }
  | texpr sep_seq
      { match $2 with
        | TmConst(_,CUnit) -> $1
        | _ -> TmApp(na,TmLam(na,"_",$2),$1) }
  | seq_aux { $1 }

sep_seq:
  | sep_texpr { $1 }
  | sep_texpr sep_seq
      { match $2 with
        | TmConst(_,CUnit) -> $1
        | _ -> TmApp(na,TmLam(na,"_",$2),$1) }
  | seq_aux { $1 }

seq_aux:
  | FUNC FUNIDENT params RPAREN texpr sep_seq
      { TmApp(na,TmLam(na,$2,$6), addrec $2 (mkfun $3 $5)) }

  | FUNC FUNIDENT RPAREN texpr sep_seq
      { TmApp(na,TmLam(na,$2,$5), addrec $2 (mkfun [""] $4)) }

  | IDENT EQ texpr sep_seq
      { TmApp(na,TmLam(na,$1,$4),$3) }

  | IDENT TILDE texpr sep_seq
      { let sample = TmSample(na,None,None) in
        TmApp(na,TmLam(na,$1,$4),TmApp(na,sample,$3)) }

texpr:
  | expr { $1 }
  | expr COMMA exprs_comma { TmTup(na,Array.of_list ($1 :: $3)) }

sep_texpr:
  | sep_expr { $1 }
  | sep_expr COMMA exprs_comma { TmTup(na,Array.of_list ($1 :: $3)) }

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
  | expr EQUAL expr     { TmApp(na,TmApp(na,TmConst(na,CEq(None)),$1),$3) }
  | expr NOTEQUAL expr  { TmApp(na,TmApp(na,TmConst(na,CNeq(None)),$1),$3) }
  | expr SHIFTLL expr   { TmApp(na,TmApp(na,TmConst(na,CSll(None)),$1),$3) }
  | expr SHIFTRL expr   { TmApp(na,TmApp(na,TmConst(na,CSrl(None)),$1),$3) }
  | expr SHIFTRA expr   { TmApp(na,TmApp(na,TmConst(na,CSra(None)),$1),$3) }
  | expr AND expr       { TmApp(na,TmApp(na,TmConst(na,CAnd(None)),$1),$3) }
  | expr OR expr        { TmApp(na,TmApp(na,TmConst(na,COr(None)),$1),$3) }
  | expr DOT IDENT             { TmRecProj(na,$1,$3) }
  | expr DOT LPAREN INT RPAREN { TmTupProj(na,$1,$4-1) }
  | expr CONCAT expr           { TmApp(na,TmApp(na,TmConcat(na,None),$1),$3) }

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
  | sep_expr ADD expr  { TmApp(na,TmApp(na,TmConst(na,CAdd(None)),$1),$3) }
  | sep_expr SUB expr  { TmApp(na,TmApp(na,TmConst(na,CSub(None)),$1),$3) }
  | sep_expr MUL expr  { TmApp(na,TmApp(na,TmConst(na,CMul(None)),$1),$3) }
  | sep_expr DIV expr  { TmApp(na,TmApp(na,TmConst(na,CDiv(None)),$1),$3) }
  | sep_expr MOD expr  { TmApp(na,TmApp(na,TmConst(na,CMod(None)),$1),$3) }
  | sep_expr LESS expr { TmApp(na,TmApp(na,TmConst(na,CLt(None)),$1),$3) }
  | sep_expr LESSEQUAL expr
      { TmApp(na,TmApp(na,TmConst(na,CLeq(None)),$1),$3) }
  | sep_expr GREAT expr { TmApp(na,TmApp(na,TmConst(na,CGt(None)),$1),$3)}
  | sep_expr GREATEQUAL expr
      { TmApp(na,TmApp(na,TmConst(na,CGeq(None)),$1),$3) }
  | sep_expr EQUAL expr { TmApp(na,TmApp(na,TmConst(na,CEq(None)),$1),$3) }
  | sep_expr NOTEQUAL expr { TmApp(na,TmApp(na,TmConst(na,CNeq(None)),$1),$3) }
  | sep_expr SHIFTLL expr { TmApp(na,TmApp(na,TmConst(na,CSll(None)),$1),$3) }
  | sep_expr SHIFTRL expr { TmApp(na,TmApp(na,TmConst(na,CSrl(None)),$1),$3) }
  | sep_expr SHIFTRA expr { TmApp(na,TmApp(na,TmConst(na,CSra(None)),$1),$3) }
  | sep_expr AND expr     { TmApp(na,TmApp(na,TmConst(na,CAnd(None)),$1),$3) }
  | sep_expr OR expr      { TmApp(na,TmApp(na,TmConst(na,COr(None)),$1),$3) }
  | sep_expr DOT IDENT             { TmRecProj(na,$1,$3) }
  | sep_expr DOT LPAREN INT RPAREN { TmTupProj(na,$1,$4-1) }
  | sep_expr CONCAT expr     { TmApp(na,TmApp(na,TmConcat(na,None),$1),$3) }

  | expr_aux { $1 }

expr_aux:
  | UTEST expr sep_expr %prec LOW
      { let a = { na with pos = Parsing.symbol_start_pos () } in
        TmUtest(a,$2,$3) }

  | OBSERVE texpr TILDE expr %prec LOW
      { let logpdf = TmLogPdf(na,None) in
        let v = $2 in
        let inner = TmApp(na,TmApp(na,logpdf,v),$4) in
        let weight = TmWeight(na,None,None) in
        TmApp(na,weight,inner) }

  | IF seq THEN seq ELSE expr %prec LOW
      { TmApp(na,TmApp(na,TmApp(na,TmIf(na,None,None),$2),TmLam(na,"",$4)),
                 TmLam(na,"",$6)) }

  | LAM params RPAREN expr %prec LOW { (mkfun $2 $4) }
  | LAM RPAREN expr %prec LOW        { (mkfun ["_"] $3) }

  | FUNIDENT exprs_comma RPAREN { mkapps (List.rev $2) $1 }
  | FUNIDENT RPAREN             { mkapps [nop] $1 }

  | MATCH seq WITH cases { TmMatch(na,$2,$4) }

  | LPAREN seq RPAREN  { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { TmRec(na,$2) }

  | LSQUARE texprs_semicolon RSQUARE { TmList(na,$2) }
  | LSQUARE RSQUARE { TmList(na,[]) }

  | LCURLY seq RCURLY  { $2 }

  | NOT expr   { TmApp(na,TmConst(na,CNot),$2) }
  | IDENT      { TmVar(na,$1,noidx) }
  | CHAR       { TmConst(na,CChar($1)) }
  | STRING     { TmConst(na,CString($1)) }
  | INT        { TmConst(na,CInt($1)) }
  | FLOAT      { TmConst(na,CFloat($1)) }
  | TRUE       { TmConst(na,CBool(true)) }
  | FALSE      { TmConst(na,CBool(false)) }

record:
  | IDENT COLON seq { [($1,$3)] }
  | IDENT COLON seq SEMICOLON record { ($1,$3) :: $5 }

params:
  | IDENT { [$1] }
  | IDENT COMMA params { $1 :: $3 }

exprs_comma:
  | expr { [$1] }
  | expr COMMA exprs_comma { $1 :: $3 }

texprs_semicolon:
  | texpr { [$1] }
  | texpr SEMICOLON texprs_semicolon { $1 :: $3 }

cases:
  | VBAR tpattern RARROW expr %prec LOW { [($2,$4)] }
  | VBAR tpattern RARROW expr cases     { ($2,$4) :: $5 }

tpattern:
  | pattern                       { $1 }
  | pattern COMMA patterns_comma  { PatTup($1 :: $3) }

pattern:
  | LPAREN tpattern RPAREN             { $2 }
  | IDENT                              { PatVar($1) }
  | LCURLY pattern_rec RCURLY          { PatRec($2) }
  | LSQUARE tpatterns_semicolon RSQUARE { PatList($2) }
  | LSQUARE RSQUARE                    { PatList([]) }
  | pattern DCOLON pattern             { PatCons($1,$3) }
  | LPAREN RPAREN                      { PatUnit }
  | CHAR                               { PatChar($1) }
  | STRING                             { PatString($1) }
  | INT                                { PatInt($1) }
  | FLOAT                              { PatFloat($1) }

pattern_rec:
  | IDENT                                     { [($1,PatVar($1))] }
  | IDENT COLON pattern                       { [($1,$3)] }
  | IDENT SEMICOLON pattern_rec               { ($1,PatVar($1)) :: $3 }
  | IDENT COLON pattern SEMICOLON pattern_rec { ($1,$3) :: $5 }

patterns_comma:
  | pattern { [$1] }
  | pattern COMMA patterns_comma { $1 :: $3 }

tpatterns_semicolon:
  | tpattern { [$1] }
  | tpattern SEMICOLON tpatterns_semicolon { $1 :: $3 }
