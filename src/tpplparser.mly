/* Parser for TreePPL. */

%{

open Ast
open Parserutils

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

%left OR
%left AND
%left LESS LESSEQUAL GREAT GREATEQUAL EQUAL NOTEQUAL
%left SHIFTLL SHIFTRL SHIFTRA
%left CONCAT
%right COLON
%left ADD SUB
%left MUL DIV MOD
%left NOT

%nonassoc DOT VBAR

%type <Ast.tm> main

%%

main:
  | seq EOF { $1 }
  | EOF { nop }

/* ************************** TREEPPL ********************************* */

seq:
  | texpr { $1 }

  | texpr seq
      { match $2 with
        | TmConst(_,CUnit) -> $1
        | _ -> TmApp(na,TmLam(na,"_",$2),$1) }

  | FUNC FUNIDENT params RPAREN texpr seq
      { TmApp(na,TmLam(na,$2,$6), addrec $2 (mkfun $3 $5)) }

  | FUNC FUNIDENT RPAREN texpr seq
      { TmApp(na,TmLam(na,$2,$5), addrec $2 (mkfun ["_"] $4)) }

  | IDENT EQ texpr seq
      { TmApp(na,TmLam(na,$1,$4),$3) }

  | IDENT TILDE texpr seq
      { let sample = TmSample(na,None,None) in
        TmApp(na,TmLam(na,$1,$4),TmApp(na,sample,$3)) }

texpr:
  | usubexpr COMMA usubexprs_comma { TmTup(na,Array.of_list ($1 :: $3)) }
  | usubexpr { $1 }

usubexpr:
  | SUB expr             { TmApp(na,TmConst(na,CNeg),$2) }
  | expr %prec LOW       { $1 }

expr:
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

  | UTEST expr expr %prec LOW
      { let a = { na with pos = Parsing.symbol_start_pos () } in
        TmApp(na,TmApp(na,TmUtest(a,None),$2),$3) }

  | OBSERVE texpr TILDE expr %prec LOW
      { let logpdf = TmLogPdf(na,None) in
        let v = $2 in
        let inner = TmApp(na,TmApp(na,logpdf,v),$4) in
        let weight = TmWeight(na,None,None) in
        TmApp(na,weight,inner) }

  | IF seq THEN seq ELSE expr %prec LOW
       { TmIf(na, $2, $4, $6) }

  | LAM params RPAREN expr %prec LOW { (mkfun $2 $4) }
  | LAM RPAREN expr %prec LOW        { (mkfun ["_"] $3) }

  | FUNIDENT usubexprs_comma RPAREN { mkapps (List.rev $2) $1 }
  | FUNIDENT RPAREN             { mkapps [nop] $1 }

  | MATCH seq WITH cases { TmMatch(na,$2,$4) }

  | LPAREN seq RPAREN  { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { TmRec(na,$2) }

  | LSQUARE usubexprs_comma RSQUARE { TmList(na,$2) }
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

usubexprs_comma:
  | usubexpr { [$1] }
  | usubexpr COMMA usubexprs_comma { $1 :: $3 }

record:
  | IDENT COLON usubexpr { [($1,$3)] }
  | IDENT COLON usubexpr COMMA record { ($1,$3) :: $5 }

params:
  | IDENT { [$1] }
  | IDENT COMMA params { $1 :: $3 }

cases:
  | VBAR tpattern RARROW expr %prec LOW { [($2,$4)] }
  | VBAR tpattern RARROW expr cases     { ($2,$4) :: $5 }

tpattern:
  | pattern                       { $1 }
  | pattern COMMA patterns_comma  { PatTup($1 :: $3) }

pattern:
  | LPAREN tpattern RPAREN              { $2 }
  | IDENT                               { PatVar($1) }
  | LCURLY pattern_rec RCURLY           { PatRec($2) }
  | LSQUARE patterns_comma RSQUARE      { PatList($2) }
  | LSQUARE RSQUARE                     { PatList([]) }
  | pattern COLON pattern               { PatCons($1,$3) }
  | LPAREN RPAREN                       { PatUnit }
  | CHAR                                { PatChar($1) }
  | STRING                              { PatString($1) }
  | INT                                 { PatInt($1) }
  | FLOAT                               { PatFloat($1) }

pattern_rec:
  | IDENT                                 { [($1,PatVar($1))] }
  | IDENT COLON pattern                   { [($1,$3)] }
  | IDENT COMMA pattern_rec               { ($1,PatVar($1)) :: $3 }
  | IDENT COLON pattern COMMA pattern_rec { ($1,$3) :: $5 }

patterns_comma:
  | pattern { [$1] }
  | pattern COMMA patterns_comma  { $1 :: $3 }


