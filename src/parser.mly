/* Parser for PPLCore. */

%{

open Ast
open ParserUtils

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
%token EQUAL         /* "="  */
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
%token NOTEQUAL      /* "!=" */
%token NOT           /* "!"   */
%token OR            /* "||" */
%token AND           /* "&&" */
%token CONCAT        /* "++" */

/* Start symbol */
%start main

/* Associativity */

%nonassoc LET LAM MATCH
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

%type <Ast.term> main

%%

main:
  | seq EOF { $1 }
  | EOF { nop }

/* ************************** PPLCORE ********************************* */

seq:

  | LET IDENT EQUAL seq IN seq %prec LET
      { mkapp ~t1:(mklam ~x:$2 ~t1:$6) ~t2:$4 }

  | LET IDENT params EQUAL seq IN seq %prec LET
      { mkapp ~t1:(mklam ~x:$2 ~t1:$7) ~t2:(addrec $2 (mkfun $3 $5)) }

  | MATCH seq WITH cases { mkapp ~t1:(mkmatch ~cls:$4) ~t2:$2 }

  | IF seq THEN seq ELSE seq %prec IF { mkapp ~t1:(mkif ~t1:$4 ~t2:$6) ~t2:$2 }

  | LAM params DOT seq %prec LAM { mkfun $2 $4 }

  | seq SEMICOLON { $1 }
  | seq SEMICOLON seq { mkapp ~t1:(mklam ~x:"_" ~t1:$3) ~t2:$1 }

  | texpr { $1 }

texpr:
  | expr COMMA exprs_comma { mktup ($1::$3) }
  | expr { $1 }

expr:
  | app { $1 }

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
  | expr COLON expr      { mkvalapps (VCons{v1=None})   [$1;$3] }
  | SUB expr             { mkvalapps VNeg               [$2]    }

app:
  | app atom { mkapp ~t1:$1 ~t2:$2 }
  | atom { $1 }

atom:
  | IDENT { mkvar ~x:$1 ~i:noidx }

  | UTEST
    { let pos = Parsing.symbol_start_pos () in
      tm_of_val' (VUtest{pos;v1=None}) }

  | LPAREN seq RPAREN { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { mkrecord $2 }

  | LSQUARE exprs_comma RSQUARE { mklist $2 }
  | LSQUARE RSQUARE { tm_of_val' (VList{vls=[]}) }

  | CHAR       { tm_of_val' (VChar{c=$1}) }
  | STRING     { tm_of_val' (VString{s=$1}) }
  | INT        { tm_of_val' (VInt{i=$1}) }
  | FLOAT      { tm_of_val' (VFloat{f=$1}) }
  | TRUE       { tm_of_val' (VBool{b=true}) }
  | FALSE      { tm_of_val' (VBool{b=false}) }

  | NOT atom   { mkapp ~t1:(tm_of_val' VNot) ~t2:$2 }

  | atom DOT IDENT
      { mkapp ~t1:(tm_of_val' (VRecProj{k=$3})) ~t2:$1 }
  | atom DOT LPAREN INT RPAREN
      { mkapp ~t1:(tm_of_val' (VTupProj{i=$4-1})) ~t2:$1 }

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
  | VBAR tuplepattern RARROW seq %prec MATCH { [($2,$4)] }
  | VBAR tuplepattern RARROW seq cases       { ($2,$4) :: $5 }

tuplepattern:
  | pattern                       { $1 }
  | pattern COMMA patterns_comma  { PTup($1 :: $3) }

pattern:
  | LPAREN tuplepattern RPAREN          { $2 }
  | IDENT                               { PVar($1) }
  | LCURLY pattern_rec RCURLY           { PRec($2) }
  | LSQUARE patterns_comma RSQUARE      { PList($2) }
  | LSQUARE RSQUARE                     { PList([]) }
  | pattern COLON pattern               { PCons($1,$3) }
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
