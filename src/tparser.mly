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

  | texpr seq
      { match $2 with
        | TVal{v=VUnit _;_} -> $1
        | _ -> TApp{at=ta;t1=TLam{at=ta;vat=xa;x="_";t1=$2};t2=$1} }

  | FUNC IDENT LPAREN params RPAREN texpr seq
      { TApp{at=ta;t1=TLam{at=ta;vat=xa;x=$2;t1=$7};
             t2=addrec $2 (mkfun $4 $6)} }

  | FUNC IDENT LPAREN RPAREN texpr seq
      { TApp{at=ta;t1=TLam{at=ta;vat=xa;x=$2;t1=$6};
             t2=addrec $2 (mkfun ["_"] $5)} }

  | IDENT EQ texpr seq
      { TApp{at=ta;t1=TLam{at=ta;vat=xa;x=$1;t1=$4};t2=$3} }

  | IDENT TILDE texpr seq
      { TApp{at=ta;t1=TLam{at=ta;vat=xa;x=$1;t1=$4};
             t2=TApp{at=ta;t1=TVal{at=ta;v=VSample{at=va}};t2=$3}} }

texpr:
  | usubexpr COMMA usubexprs_comma { mktup ($1::$3) }
  | usubexpr { $1 }

usubexpr:
  | SUB expr             { mkvalapps (VNeg{at=va}) [$2] }
  | expr %prec LOW       { $1 }

expr:

  | expr ADD expr        { mkvalapps (VAdd{at=va;v1=None})    [$1;$3] }
  | expr SUB expr        { mkvalapps (VSub{at=va;v1=None})    [$1;$3] }
  | expr MUL expr        { mkvalapps (VMul{at=va;v1=None})    [$1;$3] }
  | expr DIV expr        { mkvalapps (VDiv{at=va;v1=None})    [$1;$3] }
  | expr MOD expr        { mkvalapps (VMod{at=va;i1=None})    [$1;$3] }
  | expr LESS expr       { mkvalapps (VLt{at=va;v1=None})     [$1;$3] }
  | expr LESSEQUAL expr  { mkvalapps (VLeq{at=va;v1=None})    [$1;$3] }
  | expr GREAT expr      { mkvalapps (VGt{at=va;v1=None})     [$1;$3] }
  | expr GREATEQUAL expr { mkvalapps (VGeq{at=va;v1=None})    [$1;$3] }
  | expr EQUAL expr      { mkvalapps (VEq{at=va;v1=None})     [$1;$3] }
  | expr NOTEQUAL expr   { mkvalapps (VNeq{at=va;v1=None})    [$1;$3] }
  | expr SHIFTLL expr    { mkvalapps (VSll{at=va;i1=None})    [$1;$3] }
  | expr SHIFTRL expr    { mkvalapps (VSrl{at=va;i1=None})    [$1;$3] }
  | expr SHIFTRA expr    { mkvalapps (VSra{at=va;i1=None})    [$1;$3] }
  | expr AND expr        { mkvalapps (VAnd{at=va;b1=None})    [$1;$3] }
  | expr OR expr         { mkvalapps (VOr{at=va;b1=None})     [$1;$3] }
  | expr CONCAT expr     { mkvalapps (VConcat{at=va;v1=None}) [$1;$3] }
  | expr DCOLON expr     { mkvalapps (VCons{at=va;v1=None})   [$1;$3] }

  | expr DOT IDENT
      { TApp{at=ta;t1=TVal{at=ta;v=VRecProj{at=va;k=$3}};t2=$1} }
  | expr DOT LPAREN INT RPAREN
      { TApp{at=ta;t1=TVal{at=ta;v=VTupProj{at=va;i=$4-1}};t2=$1} }

  | UTEST expr expr %prec LOW
      { let pos = Parsing.symbol_start_pos () in
        mkvalapps (VUtest{at=va;pos=pos;v1=None}) [$2;$3] }

  | OBSERVE texpr TILDE expr %prec LOW
      { let logpdf = TVal{at=ta;v=VLogPdf{at=va;v1=None}} in
        let v = $2 in
        let inner = TApp{at=ta;t1=TApp{at=ta;t1=logpdf;t2=v};t2=$4} in
        let weight = TVal{at=ta;v=VWeight{at=va}} in
        TApp{at=ta;t1=weight;t2=inner} }

  | IF seq THEN seq ELSE usubexpr
       { TApp{at=ta;t1=TIf{at=ta;t1=$4;t2=$6};t2=$2} }

  | LAM LPAREN params RPAREN usubexpr { (mkfun $3 $5) }
  | LAM LPAREN RPAREN usubexpr        { (mkfun ["_"] $4) }

  | IDENT LPAREN usubexprs_comma RPAREN
      { mkapps (TVar{at=ta;vat=xa;x=$1;i=noidx}) $3 }
  | IDENT LPAREN RPAREN
      { mkapps (TVar{at=ta;vat=xa;x=$1;i=noidx}) [nop] }

  | MATCH seq WITH cases { TApp{at=ta;t1=TMatch{at=ta;cls=$4};t2=$2} }

  | LPAREN seq RPAREN  { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { mkrecord $2 }

  | LSQUARE usubexprs_comma RSQUARE { mklist $2 }
  | LSQUARE RSQUARE { TVal{at=ta;v=VList{at=va;vls=[]}} }

  | LCURLY seq RCURLY  { $2 }

  | NOT expr            { TApp{at=ta;t1=TVal{at=ta;v=VNot{at=va}};t2=$2} }
  | IDENT %prec LOW     { TVar{at=ta;vat=xa;x=$1;i=noidx} }
  | CHAR                { TVal{at=ta;v=VChar{at=va;c=$1}} }
  | STRING              { TVal{at=ta;v=VString{at=va;s=$1}} }
  | INT                 { TVal{at=ta;v=VInt{at=va;i=$1}} }
  | FLOAT               { TVal{at=ta;v=VFloat{at=va;f=$1}} }
  | TRUE                { TVal{at=ta;v=VBool{at=va;b=true}} }
  | FALSE               { TVal{at=ta;v=VBool{at=va;b=false}} }

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


