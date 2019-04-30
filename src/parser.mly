%{
(* TODO The first special comment of the file is the comment associated with
   the whole module. *)

open Ast
open Parserutils

%}

/* Misc */
%token EOF
%token <string> IDENT

/* Keywords */
%token LAM
%token IF
%token THEN
%token ELSE
%token UTEST
%token OBSERVE
%token MATCH
%token WITH
%token LET
%token IN

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

/* Start symbol */
%start main

/* Associativity */

%nonassoc OBSERVE LET MATCH
%nonassoc VBAR

%right SEMICOLON

%nonassoc IF

%left OR
%left AND
%left EQUAL NOTEQUAL
%left LESS LESSEQUAL GREAT GREATEQUAL
%left SHIFTLL SHIFTRL SHIFTRA
%left CONCAT
%right COLON
%left ADD SUB
%left MUL DIV MOD

%nonassoc NOT DOT

%type <Ast.tm> main

%%

main:
  | seq EOF { $1 }
  | EOF { nop }

/* ************************** PPLCORE ********************************* */

seq:
  | seq SEMICOLON { $1 }
  | seq SEMICOLON seq  { TmApp(na,TmLam(na,"_",$3),$1) }
  | texpr { $1 }

texpr:
  | expr COMMA exprs_comma { TmTup(na,Array.of_list ($1 :: $3)) }
  | expr { $1 }

expr:
  | app { $1 }

  | expr ADD expr        { TmApp(na,TmApp(na,TmConst(na,CAdd(None)),$1),$3) }
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
  | expr CONCAT expr     { TmApp(na,TmApp(na,TmConcat(na,None),$1),$3) }
  | expr SUB expr        { TmApp(na,TmApp(na,TmConst(na,CSub(None)),$1),$3) }

  | OBSERVE seq TILDE expr %prec OBSERVE
      { let logpdf = TmLogPdf(na,None) in
        let v = $2 in
        let inner = TmApp(na,TmApp(na,logpdf,v),$4) in
        let weight = TmWeight(na,None,None) in
        TmApp(na,weight,inner) }

  | SUB expr { TmApp(na,TmConst(na,CNeg),$2) }

  | LAM params DOT expr { mkfun $2 $4 }
  | LET IDENT EQ seq IN expr %prec LET { TmApp(na,TmLam(na,$2,$6),$4) }
  | LET IDENT params EQ seq IN expr %prec LET
       { TmApp(na,TmLam(na,$2,$7), addrec $2 (mkfun $3 $5)) }

  | LET IDENT TILDE seq IN expr %prec LET
       { let sample = TmSample(na,None,None) in
        TmApp(na,TmLam(na,$2,$6),TmApp(na,sample,$4)) }

  | IF seq THEN seq ELSE expr %prec IF
       { TmApp(na,TmApp(na,TmApp(na,TmIf(na,None,None),$2),TmLam(na,"",$4)),
                 TmLam(na,"",$6)) }

  | MATCH seq WITH cases { TmMatch(na,$2,$4) }

app:
  | app atom { TmApp(na,$1,$2) }
  | atom { $1 }

atom:
  | IDENT { TmVar(na,$1,noidx) }

  | UTEST { let a = { na with pos = Parsing.symbol_start_pos () } in
            TmUtest(a,None) }

  | LPAREN seq RPAREN { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { TmRec(na,$2) }

  | LSQUARE exprs_comma RSQUARE { TmList(na,$2) }
  | LSQUARE RSQUARE { TmList(na,[]) }

  | CHAR       { TmConst(na,CChar($1)) }
  | STRING     { TmConst(na,CString($1)) }
  | INT        { TmConst(na,CInt($1)) }
  | FLOAT      { TmConst(na,CFloat($1)) }
  | TRUE       { TmConst(na,CBool(true)) }
  | FALSE      { TmConst(na,CBool(false)) }

  | NOT atom   { TmApp(na,TmConst(na,CNot),$2) }

  | atom DOT IDENT             { TmRecProj(na,$1,$3) }
  | atom DOT LPAREN INT RPAREN { TmTupProj(na,$1,$4-1) }

exprs_comma:
  | expr { [$1] }
  | expr COMMA exprs_comma { $1 :: $3 }

record:
  | IDENT COLON expr { [($1,$3)] }
  | IDENT COLON expr COMMA record { ($1,$3) :: $5 }

params:
  | IDENT { [$1] }
  | IDENT params { $1 :: $2 }

cases:
  | VBAR tuplepattern RARROW expr %prec MATCH { [($2,$4)] }
  | VBAR tuplepattern RARROW expr cases       { ($2,$4) :: $5 }

tuplepattern:
  | pattern                       { $1 }
  | pattern COMMA patterns_comma  { PatTup($1 :: $3) }

pattern:
  | LPAREN tuplepattern RPAREN          { $2 }
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


