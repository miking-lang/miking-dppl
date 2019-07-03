/* Parser for TreePPL. */

%{

open Ast
open ParserUtils

%}

/* Misc */
%token EOF
%token <string> IDENT

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
%right DCOLON
%left ADD SUB
%left MUL DIV MOD
%left NOT

%left LPAREN   /* Applications bind tightly => we should always shift LPAREN */

%nonassoc DOT VBAR

%type <Ast.term> main

%%

main:
  | seq EOF { $1 }
  | EOF { nop }

/* ************************** TREEPPL ********************************* */

seq:
  | texpr { $1 }

  | texpr seq { mkapp ~t1:(mklam ~x:"_" ~t1:$2) ~t2:$1 }

  | FUNC IDENT LPAREN params RPAREN texpr seq
      { mkapp ~t1:(mklam ~x:$2 ~t1:$7) ~t2:(addrec $2 (mkfun $4 $6)) }

  | FUNC IDENT LPAREN RPAREN texpr seq
      { mkapp ~t1:(mklam ~x:$2 ~t1:$6) ~t2:(addrec $2 (mkfun ["_"] $5)) }

  | IDENT EQ texpr seq { mkapp ~t1:(mklam ~x:$1 ~t1:$4) ~t2:$3 }

  | IDENT TILDE texpr seq
      { let sample = tm_of_val' (VSample{cont=None;d=None}) in
        mkapp ~t1:(mklam ~x:$1 ~t1:$4) ~t2:(mkapp ~t1:sample ~t2:$3) }

texpr:
  | usubexpr COMMA usubexprs_comma { mktup ($1::$3) }
  | usubexpr { $1 }

usubexpr:
  | SUB expr             { mkvalapps VNeg [$2] }
  | expr %prec LOW       { $1 }

expr:

  | expr ADD expr        { mkvalapps (VAdd{v1=None})    [$1;$3] }
  | expr SUB expr        { mkvalapps (VSub{v1=None})    [$1;$3] }
  | expr MUL expr        { mkvalapps (VMul{v1=None})    [$1;$3] }
  | expr DIV expr        { mkvalapps (VDiv{v1=None})    [$1;$3] }
  | expr MOD expr        { mkvalapps (VMod{i1=None})    [$1;$3] }
  | expr LESS expr       { mkvalapps (VLt{v1=None})     [$1;$3] }
  | expr LESSEQUAL expr  { mkvalapps (VLeq{v1=None})    [$1;$3] }
  | expr GREAT expr      { mkvalapps (VGt{v1=None})     [$1;$3] }
  | expr GREATEQUAL expr { mkvalapps (VGeq{v1=None})    [$1;$3] }
  | expr EQUAL expr      { mkvalapps (VEq{v1=None})     [$1;$3] }
  | expr NOTEQUAL expr   { mkvalapps (VNeq{v1=None})    [$1;$3] }
  | expr SHIFTLL expr    { mkvalapps (VSll{i1=None})    [$1;$3] }
  | expr SHIFTRL expr    { mkvalapps (VSrl{i1=None})    [$1;$3] }
  | expr SHIFTRA expr    { mkvalapps (VSra{i1=None})    [$1;$3] }
  | expr AND expr        { mkvalapps (VAnd{b1=None})    [$1;$3] }
  | expr OR expr         { mkvalapps (VOr{b1=None})     [$1;$3] }
  | expr CONCAT expr     { mkvalapps (VConcat{v1=None}) [$1;$3] }
  | expr DCOLON expr     { mkvalapps (VCons{v1=None})   [$1;$3] }

  | expr DOT IDENT
      { mkapp ~t1:(tm_of_val' (VRecProj{k=$3})) ~t2:$1 }
  | expr DOT LPAREN INT RPAREN
      { mkapp ~t1:(tm_of_val' (VTupProj{i=$4-1})) ~t2:$1 }

  | UTEST expr expr %prec LOW
      { let pos = Parsing.symbol_start_pos () in
        mkvalapps (VUtest{pos=pos;v1=None}) [$2;$3] }

  | OBSERVE texpr TILDE expr %prec LOW
      { let logpdf = tm_of_val' (VLogPdf{v1=None}) in
        let v = $2 in
        let inner = mkapp ~t1:(mkapp ~t1:logpdf ~t2:v) ~t2:$4 in
        let weight = tm_of_val' (VWeight{cont=None;w=None}) in
        mkapp ~t1:weight ~t2:inner }

  | IF seq THEN seq ELSE usubexpr
       { mkapp ~t1:(mkif ~t1:$4 ~t2:$6) ~t2:$2 }

  | LAM LPAREN params RPAREN usubexpr { (mkfun $3 $5) }
  | LAM LPAREN RPAREN usubexpr        { (mkfun ["_"] $4) }

  | IDENT LPAREN usubexprs_comma RPAREN
      { mkapps (mkvar ~x:$1 ~i:noidx) $3 }
  | IDENT LPAREN RPAREN
      { mkapps (mkvar ~x:$1 ~i:noidx) [nop] }

  | MATCH seq WITH cases { mkapp ~t1:(mkmatch ~cls:$4) ~t2:$2 }

  | LPAREN seq RPAREN  { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { mkrecord $2 }

  | LSQUARE usubexprs_comma RSQUARE { mklist $2 }
  | LSQUARE RSQUARE { tm_of_val' (VList{vls=[]}) }

  | LCURLY seq RCURLY  { $2 }

  | NOT expr            { mkapp ~t1:(tm_of_val' VNot) ~t2:$2 }
  | IDENT %prec LOW     { mkvar ~x:$1 ~i:noidx }
  | CHAR                { tm_of_val' (VChar{c=$1}) }
  | STRING              { tm_of_val' (VString{s=$1}) }
  | INT                 { tm_of_val' (VInt{i=$1}) }
  | FLOAT               { tm_of_val' (VFloat{f=$1}) }
  | TRUE                { tm_of_val' (VBool{b=true}) }
  | FALSE               { tm_of_val' (VBool{b=false}) }

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
  | VBAR tuplepattern RARROW expr %prec LOW { [($2,$4)] }
  | VBAR tuplepattern RARROW expr cases     { ($2,$4) :: $5 }

tuplepattern:
  | pattern                       { $1 }
  | pattern COMMA patterns_comma  { PTup($1 :: $3) }

pattern:
  | LPAREN tuplepattern RPAREN          { $2 }
  | IDENT                               { PVar($1) }
  | LCURLY pattern_rec RCURLY           { PRec($2) }
  | LSQUARE patterns_comma RSQUARE      { PList($2) }
  | LSQUARE RSQUARE                     { PList([]) }
  | pattern DCOLON pattern              { PCons($1,$3) }
  | LPAREN RPAREN                       { PUnit }
  | CHAR                                { PChar($1) }
  | STRING                              { PString($1) }
  | INT                                 { PInt($1) }
  | FLOAT                               { PFloat($1) }

pattern_rec:
  | IDENT                                 { [($1,PVar($1))] }
  | IDENT COLON pattern                   { [($1,$3)] }
  | IDENT COMMA pattern_rec               { ($1,PVar($1)) :: $3 }
  | IDENT COLON pattern COMMA pattern_rec { ($1,$3) :: $5 }

patterns_comma:
  | pattern { [$1] }
  | pattern COMMA patterns_comma  { $1 :: $3 }


