/* Parser for PPLCore. */

%{

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

%nonassoc OBSERVE LET LAM MATCH
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
  | LET IDENT EQUAL seq IN seq %prec LET { TApp(na,TLam(na,$2,$6),$4) }
  | LET IDENT params EQUAL seq IN seq %prec LET
       { TApp(na,TLam(na,$2,$7), addrec $2 (mkfun $3 $5)) }

  | LET IDENT TILDE seq IN seq %prec LET
       { TApp(na,TLam(na,$2,$6),TApp(na,TVal(VSample na),$4)) }

  | OBSERVE seq TILDE seq %prec OBSERVE
      { let logpdf = TVal(VLogPdf(na,None)) in
        let v = $2 in
        let inner = TApp(na,TApp(na,logpdf,v),$4) in
        let weight = TVal(VWeight na) in
        TApp(na,weight,inner) }

  | MATCH seq WITH cases { TApp(na,TMatch(na,$4),$2) }

  | IF seq THEN seq ELSE seq %prec IF { TApp(na,TIf(na,$4,$6),$2) }

  | LAM params DOT seq %prec LAM { mkfun $2 $4 }

  | seq SEMICOLON { $1 }
  | seq SEMICOLON seq  { TApp(na,TLam(na,"_",$3),$1) }

  | texpr { $1 }

texpr:
  | expr COMMA exprs_comma { mktup ($1::$3) }
  | expr { $1 }

expr:
  | app { $1 }

  | expr ADD expr        { TApp(na,TApp(na,TVal(VAdd(na,None)),$1),$3) }
  | expr MUL expr        { TApp(na,TApp(na,TVal(VMul(na,None)),$1),$3) }
  | expr DIV expr        { TApp(na,TApp(na,TVal(VDiv(na,None)),$1),$3) }
  | expr MOD expr        { TApp(na,TApp(na,TVal(VMod(na,None)),$1),$3) }
  | expr LESS expr       { TApp(na,TApp(na,TVal(VLt(na,None)),$1),$3) }
  | expr LESSEQUAL expr  { TApp(na,TApp(na,TVal(VLeq(na,None)),$1),$3) }
  | expr GREAT expr      { TApp(na,TApp(na,TVal(VGt(na,None)),$1),$3)}
  | expr GREATEQUAL expr { TApp(na,TApp(na,TVal(VGeq(na,None)),$1),$3) }
  | expr EQUAL expr      { TApp(na,TApp(na,TVal(VEq(na,None)),$1),$3) }
  | expr NOTEQUAL expr   { TApp(na,TApp(na,TVal(VNeq(na,None)),$1),$3) }
  | expr SHIFTLL expr    { TApp(na,TApp(na,TVal(VSll(na,None)),$1),$3) }
  | expr SHIFTRL expr    { TApp(na,TApp(na,TVal(VSrl(na,None)),$1),$3) }
  | expr SHIFTRA expr    { TApp(na,TApp(na,TVal(VSra(na,None)),$1),$3) }
  | expr AND expr        { TApp(na,TApp(na,TVal(VAnd(na,None)),$1),$3) }
  | expr OR expr         { TApp(na,TApp(na,TVal(VOr(na,None)),$1),$3) }
  | expr CONCAT expr     { TApp(na,TApp(na,TVal(VConcat(na,None)),$1),$3) }
  | expr SUB expr        { TApp(na,TApp(na,TVal(VSub(na,None)),$1),$3) }
  | expr COLON expr      { TApp(na,TApp(na,TVal(VCons(na,None)),$1),$3) }

  | SUB expr { TApp(na,TVal(VNeg na),$2) }

app:
  | app atom { TApp(na,$1,$2) }
  | atom { $1 }

atom:
  | IDENT { TVar(na,$1,noidx) }

  | UTEST { let a = { na with pos = Parsing.symbol_start_pos () } in
            TVal(VUtest(a,None)) }

  | LPAREN seq RPAREN { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { mkrecord $2 }

  | LSQUARE exprs_comma RSQUARE { mklist $2 }
  | LSQUARE RSQUARE { TVal(VList(na,[])) }

  | CHAR       { TVal(VChar(na,$1)) }
  | STRING     { TVal(VString(na,$1)) }
  | INT        { TVal(VInt(na,$1)) }
  | FLOAT      { TVal(VFloat(na,$1)) }
  | TRUE       { TVal(VBool(na,true)) }
  | FALSE      { TVal(VBool(na,false)) }

  | NOT atom   { TApp(na,TVal(VNot na),$2) }

  | atom DOT IDENT             { TApp(na,TVal(VRecProj(na,$3)),$1) }
  | atom DOT LPAREN INT RPAREN { TApp(na,TVal(VTupProj(na,$4-1)),$1) }

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


