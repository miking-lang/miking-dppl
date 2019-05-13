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
        | TVal(VUnit _) -> $1
        | _ -> TApp(na,TLam(na,"_",$2),$1) }

  | FUNC FUNIDENT params RPAREN texpr seq
      { TApp(na,TLam(na,$2,$6), addrec $2 (mkfun $3 $5)) }

  | FUNC FUNIDENT RPAREN texpr seq
      { TApp(na,TLam(na,$2,$5), addrec $2 (mkfun ["_"] $4)) }

  | IDENT EQ texpr seq
      { TApp(na,TLam(na,$1,$4),$3) }

  | IDENT TILDE texpr seq
      { TApp(na,TLam(na,$1,$4),TApp(na,TVal(VSample na),$3)) }

texpr:
  | usubexpr COMMA usubexprs_comma { nop (* TODO *) }
  | usubexpr { $1 }

usubexpr:
  | SUB expr             { TApp(na,TVal(VNeg na),$2) }
  | expr %prec LOW       { $1 }

expr:
  | expr ADD expr        { TApp(na,TApp(na,TVal(VAdd(na,None)),$1),$3) }
  | expr SUB expr        { TApp(na,TApp(na,TVal(VSub(na,None)),$1),$3) }
  | expr MUL expr        { TApp(na,TApp(na,TVal(VMul(na,None)),$1),$3) }
  | expr DIV expr        { TApp(na,TApp(na,TVal(VDiv(na,None)),$1),$3) }
  | expr MOD expr        { TApp(na,TApp(na,TVal(VMod(na,None)),$1),$3) }
  | expr LESS expr       { TApp(na,TApp(na,TVal(VLt(na,None)),$1),$3) }
  | expr LESSEQUAL expr  { TApp(na,TApp(na,TVal(VLeq(na,None)),$1),$3) }
  | expr GREAT expr      { TApp(na,TApp(na,TVal(VGt(na,None)),$1),$3)}
  | expr GREATEQUAL expr { TApp(na,TApp(na,TVal(VGeq(na,None)),$1),$3) }
  | expr EQUAL expr     { TApp(na,TApp(na,TVal(VEq(na,None)),$1),$3) }
  | expr NOTEQUAL expr  { TApp(na,TApp(na,TVal(VNeq(na,None)),$1),$3) }
  | expr SHIFTLL expr   { TApp(na,TApp(na,TVal(VSll(na,None)),$1),$3) }
  | expr SHIFTRL expr   { TApp(na,TApp(na,TVal(VSrl(na,None)),$1),$3) }
  | expr SHIFTRA expr   { TApp(na,TApp(na,TVal(VSra(na,None)),$1),$3) }
  | expr AND expr       { TApp(na,TApp(na,TVal(VAnd(na,None)),$1),$3) }
  | expr OR expr        { TApp(na,TApp(na,TVal(VOr(na,None)),$1),$3) }
  | expr DCOLON expr     { nop (* TODO *) }
  | expr DOT IDENT             { TApp(na,TVal(VRecProj(na,$3)),$1) }
  | expr DOT LPAREN INT RPAREN { TApp(na,TVal(VTupProj(na,$4-1)),$1) }
  | expr CONCAT expr           { TApp(na,TApp(na,TVal(VConcat(na,None)),$1),$3) }

  | UTEST expr expr %prec LOW
      { let a = { na with pos = Parsing.symbol_start_pos () } in
        TApp(na,TApp(na,TVal(VUtest(a,None)),$2),$3) }

  | OBSERVE texpr TILDE expr %prec LOW
      { let logpdf = TVal(VLogPdf(na,None)) in
        let v = $2 in
        let inner = TApp(na,TApp(na,logpdf,v),$4) in
        let weight = TVal(VWeight na) in
        TApp(na,weight,inner) }

  | IF seq THEN seq ELSE expr %prec LOW
       { TApp(na,TIf(na,$4,$6),$2) }

  | LAM params RPAREN expr %prec LOW { (mkfun $2 $4) }
  | LAM RPAREN expr %prec LOW        { (mkfun ["_"] $3) }

  | FUNIDENT usubexprs_comma RPAREN { mkapps (List.rev $2) $1 }
  | FUNIDENT RPAREN             { mkapps [nop] $1 }

  | MATCH seq WITH cases { TApp(na,TMatch(na,$4),$2) }

  | LPAREN seq RPAREN  { $2 }
  | LPAREN RPAREN { nop }

  | LCURLY record RCURLY { nop (* TODO *) }

  | LSQUARE usubexprs_comma RSQUARE { nop (* TODO *) }
  | LSQUARE RSQUARE { nop (* TODO *) }

  | LCURLY seq RCURLY  { $2 }

  | NOT expr   { TApp(na,TVal(VNot na),$2) }
  | IDENT      { TVar(na,$1,noidx) }
  | CHAR       { TVal(VChar(na,$1)) }
  | STRING     { TVal(VString(na,$1)) }
  | INT        { TVal(VInt(na,$1)) }
  | FLOAT      { TVal(VFloat(na,$1)) }
  | TRUE       { TVal(VBool(na,true)) }
  | FALSE      { TVal(VBool(na,false)) }

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


