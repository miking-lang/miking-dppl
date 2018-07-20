/*
   Miking is licensed under the MIT license.
   Copyright (C) David Broman. See file LICENSE.txt

*/

%{

  open Ustring.Op
  open Msg
  open Ast

  (** Create a new info, taking left and right part *)
  let mkinfo fi1 fi2 =
    match (fi1,fi2) with
      | (Info(fn,r1,c1,_,_), Info(_,_,_,r2,c2)) -> Info(fn,r1,c1,r2,c2)
      | (Info(fn,r1,c1,r2,c2), NoInfo) -> Info(fn,r1,c1,r2,c2)
      | (NoInfo, Info(fn,r1,c1,r2,c2)) -> Info(fn,r1,c1,r2,c2)
      | (_,_) -> NoInfo

   (** Add fix-point, if recursive function *)
  let addrec x t =
    let rec hasx t = match t with
      | TmVar(_,y,_,_) ->  x =. y
      | TmLam(_,y,t1) -> if x =. y then false else hasx t1
      | TmClos(_,_,_,_,_) -> failwith "Cannot happen"
      | TmApp(_,t1,t2) -> hasx t1 || hasx t2
      | TmConst(_,_) -> false
      | TmFix(_) -> false
      | TmPEval(_) -> false
      | TmIfexp(_,_,None) -> false
      | TmIfexp(_,_,Some(t1)) -> hasx t1
      | TmChar(_,_) -> false
      | TmExprSeq(_,t1,t2) -> hasx t1 || hasx t2
      | TmUC(fi,uct,ordered,uniqueness) ->
          let rec work uc = match uc with
          | UCNode(uc1,uc2) -> work uc1 || work uc2
          | UCLeaf(tms) -> List.exists hasx tms
          in work uct
      | TmUtest(fi,t1,t2,tnext) -> hasx t1 || hasx t2 || hasx tnext
      | TmMatch(fi,t1,cases) ->
          List.exists (fun (Case(_,_,t)) -> hasx t) cases
      | TmNop -> false
    in
    if hasx t then TmApp(NoInfo,TmFix(NoInfo), (TmLam(NoInfo,x,t))) else t


%}

/* Misc tokens */
%token EOF
%token <Ustring.ustring Ast.tokendata> IDENT
%token <Ustring.ustring Ast.tokendata> FUNIDENT
%token <Ustring.ustring Ast.tokendata> STRING
%token <Ustring.ustring Ast.tokendata> CHAR
%token <int Ast.tokendata> UINT
%token <float Ast.tokendata> UFLOAT
%token <Ustring.Op.sid Ast.tokendata> ATOM

/* Keywords */
%token <unit Ast.tokendata> FUNC
%token <unit Ast.tokendata> FUNC2
%token <unit Ast.tokendata> DEF
%token <unit Ast.tokendata> IN
%token <unit Ast.tokendata> IF
%token <unit Ast.tokendata> IF2           /* Special handling if( */
%token <unit Ast.tokendata> THEN
%token <unit Ast.tokendata> ELSE
%token <unit Ast.tokendata> TRUE
%token <unit Ast.tokendata> FALSE
%token <unit Ast.tokendata> MATCH
%token <unit Ast.tokendata> UTEST
%token <unit Ast.tokendata> TYPE
%token <unit Ast.tokendata> DATA
%token <unit Ast.tokendata> LANG
%token <unit Ast.tokendata> MCORE
%token <unit Ast.tokendata> RAGNAR
%token <unit Ast.tokendata> LET
%token <unit Ast.tokendata> LAM
%token <unit Ast.tokendata> IN
%token <unit Ast.tokendata> NOP
%token <unit Ast.tokendata> FIX
%token <unit Ast.tokendata> PEVAL
%token <unit Ast.tokendata> IFEXP
%token <unit Ast.tokendata> OBSERVE




%token <unit Ast.tokendata> EQ            /* "="  */
%token <unit Ast.tokendata> TILDE         /* "~"  */
%token <unit Ast.tokendata> ARROW         /* "->"  */
%token <unit Ast.tokendata> ADD           /* "+"  */
%token <unit Ast.tokendata> SUB           /* "-"  */
%token <unit Ast.tokendata> MUL           /* "*"  */
%token <unit Ast.tokendata> DIV           /* "/"  */
%token <unit Ast.tokendata> MOD           /* "%"  */
%token <unit Ast.tokendata> LESS          /* "<"  */
%token <unit Ast.tokendata> LESSEQUAL     /* "<=" */
%token <unit Ast.tokendata> GREAT         /* ">"  */
%token <unit Ast.tokendata> GREATEQUAL    /* ">=" */
%token <unit Ast.tokendata> SHIFTLL       /* "<<" */
%token <unit Ast.tokendata> SHIFTRL       /* ">>" */
%token <unit Ast.tokendata> SHIFTRA       /* ">>>" */
%token <unit Ast.tokendata> EQUAL         /* "==" */
%token <unit Ast.tokendata> NOTEQUAL      /* "!=" */
%token <unit Ast.tokendata> NOT           /* "!"   */
%token <unit Ast.tokendata> OR            /* "||" */
%token <unit Ast.tokendata> AND           /* "&&" */
%token <unit Ast.tokendata> CONCAT        /* "++" */



/* Symbolic Tokens */
%token <unit Ast.tokendata> LPAREN        /* "("  */
%token <unit Ast.tokendata> RPAREN        /* ")"  */
%token <unit Ast.tokendata> LSQUARE       /* "["  */
%token <unit Ast.tokendata> RSQUARE       /* "]"  */
%token <unit Ast.tokendata> LCURLY        /* "{"  */
%token <unit Ast.tokendata> RCURLY        /* "}"  */
%token <unit Ast.tokendata> CONS          /* "::" */
%token <unit Ast.tokendata> COLON         /* ":"  */
%token <unit Ast.tokendata> COMMA         /* ","  */
%token <unit Ast.tokendata> DOT           /* "."  */
%token <unit Ast.tokendata> BAR           /* "|"  */
%token <unit Ast.tokendata> ARROW         /* "->" */
%token <unit Ast.tokendata> DARROW        /* "=>" */

%start main

%nonassoc IF
%left OR
%left AND
%left LESS LESSEQUAL GREAT GREATEQUAL EQUAL NOTEQUAL
%left CONCAT
%left SHIFTLL SHIFTRL SHIFTRA
%nonassoc NOT
%left ADD SUB
%left MUL DIV MOD
%nonassoc USUB

%type <Ast.tm> main

%%

main:
  | top_treeppl_scope EOF { $1 }

/* ************************** TREEPPL ********************************* */


/*
 * Top level treeppl_scope. As an example, this enables writing
 * { -1 } instead of { (-1) }.
 */
top_treeppl_scope:
  | expr treeppl_scope
      { let fi = tm_info $1 in
        match $2 with
        | TmNop -> $1
        | _ -> TmApp(fi, TmLam(fi, us"_", $2), $1) }
  | treeppl_scope_aux { $1 }

treeppl_scope:
  | sep_expr treeppl_scope
      { let fi = tm_info $1 in
        match $2 with
        | TmNop -> $1
        | _ -> TmApp(fi, TmLam(fi, us"_", $2), $1) }
  | treeppl_scope_aux { $1 }

treeppl_scope_aux:
  | { TmNop }
  | FUNC FUNIDENT params RPAREN expr treeppl_scope
      { let fi = mkinfo $1.i (tm_info $5) in
        let rec mkfun lst =
          (match lst with
          | x::xs -> TmLam(fi,x,mkfun xs)
          | [] -> $5 ) in
        let f = if List.length $3 = 0 then [us"_"] else $3 in
        TmApp(fi,TmLam(fi,$2.v,$6), addrec $2.v (mkfun f)) }
  | IDENT EQ expr treeppl_scope
      { let fi = mkinfo $1.i (tm_info $3) in
        TmApp(fi,TmLam(fi,$1.v,$4),$3) }
  | IDENT TILDE expr treeppl_scope
      { let fi = mkinfo $1.i (tm_info $3) in
        let sample = TmVar(fi,us"sample",noidx,false) in
        TmApp(fi,TmLam(fi,$1.v,$4),TmApp(fi, sample, $3)) }
  | OBSERVE expr TILDE expr treeppl_scope
      { let fi = mkinfo $1.i (tm_info $4) in
        let prob = TmVar(fi,us"prob",noidx,false) in
        let v = $2 in
        let inner = TmApp(fi, TmApp(fi, prob, v), $4) in
        let weight = TmVar(fi,us"weight",noidx,false) in
        let outer = TmApp(fi, weight, inner) in
        TmApp(fi,TmLam(fi,us"_",$5),outer) }
  | UTEST expr sep_expr treeppl_scope
      { let fi = mkinfo $1.i (tm_info $3) in
        TmUtest(fi,$2,$3,$4) }

expr:
  | FUNIDENT exprs RPAREN
      { let fi = mkinfo $1.i $3.i in
        let rec mkapps lst =
          match lst with
          | t::ts ->  TmApp(fi,mkapps ts,t)
          | [] -> TmVar($1.i,$1.v,noidx,false)
        in
        (match Ustring.to_utf8 $1.v with
         | "seq"     -> TmUC($1.i,UCLeaf($2),UCOrdered,UCMultivalued)
         | _ -> mkapps (if List.length $2 = 0 then [TmNop] else (List.rev $2)))}
  | IF expr THEN expr ELSE expr %prec IF
      { let fi = mkinfo $1.i (tm_info $6) in
        TmApp(fi,TmApp(fi,TmApp(fi,TmIfexp(fi,None,None),$2),
              TmLam(tm_info $4,us"",$4)),
              TmLam(tm_info $6,us"",$6)) }
  | expr ADD expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cadd(TNone)),$1),$3) }
  | expr SUB expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Csub(TNone)),$1),$3) }
  | expr MUL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cmul(TNone)),$1),$3) }
  | expr DIV expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cdiv(TNone)),$1),$3) }
  | expr MOD expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cmodi(None)),$1),$3) }
  | expr LESS expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Clti(None)),$1),$3) }
  | expr LESSEQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cleqi(None)),$1),$3) }
  | expr GREAT expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cgti(None)),$1),$3)}
  | expr GREATEQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cgeqi(None)),$1),$3) }
  | expr EQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,CPolyEq(None)),$1),$3) }
  | expr NOTEQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,CPolyNeq(None)),$1),$3) }
  | expr SHIFTLL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cslli(None)),$1),$3) }
  | expr SHIFTRL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Csrli(None)),$1),$3) }
  | expr SHIFTRA expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Csrai(None)),$1),$3) }
  | NOT expr
      { TmApp($1.i,TmConst($1.i,Cnot),$2) }
  | SUB expr %prec USUB    { TmApp($1.i,TmConst($1.i,Cneg),$2) }
  | expr AND expr
            { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cand(None)),$1),$3) }
  | expr OR expr
             { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cor(None)),$1),$3) }
  | expr CONCAT expr
         { TmApp($2.i,TmApp($2.i,TmConst($2.i,CConcat(None)),$1),$3) }
  | expr_aux { $1 }

/*
 * Separable expressions for sequencing. Combined with expr,
 * Enables function calls such as
 * foo(-1, -2) instead of foo((-1), (-2)).
 * Also gives other nice properties such as being able to write
 * x = -1 instead of x = (-1). Gives a difference between writing
 * -1 -1 (evaluates to -2) and
 * -1 (-1) (evaluates to -1 because of sequencing)
 * Unfortunate with code duplication. TODO Is there a better way?
 */
sep_expr:
  | FUNIDENT exprs RPAREN
      { let fi = mkinfo $1.i $3.i in
        let rec mkapps lst =
          match lst with
          | t::ts ->  TmApp(fi,mkapps ts,t)
          | [] -> TmVar($1.i,$1.v,noidx,false)
        in
        (match Ustring.to_utf8 $1.v with
         | "seq"     -> TmUC($1.i,UCLeaf($2),UCOrdered,UCMultivalued)
         | _ -> mkapps (if List.length $2 = 0 then [TmNop] else (List.rev $2)))}
  | IF expr THEN expr ELSE expr %prec IF
      { let fi = mkinfo $1.i (tm_info $6) in
        TmApp(fi,TmApp(fi,TmApp(fi,TmIfexp(fi,None,None),$2),
              TmLam(tm_info $4,us"",$4)),
              TmLam(tm_info $6,us"",$6)) }
  | sep_expr ADD expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cadd(TNone)),$1),$3) }
  | sep_expr SUB expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Csub(TNone)),$1),$3) }
  | sep_expr MUL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cmul(TNone)),$1),$3) }
  | sep_expr DIV expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cdiv(TNone)),$1),$3) }
  | sep_expr MOD expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cmodi(None)),$1),$3) }
  | sep_expr LESS expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Clti(None)),$1),$3) }
  | sep_expr LESSEQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cleqi(None)),$1),$3) }
  | sep_expr GREAT expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cgti(None)),$1),$3)}
  | sep_expr GREATEQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cgeqi(None)),$1),$3) }
  | sep_expr EQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,CPolyEq(None)),$1),$3) }
  | sep_expr NOTEQUAL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,CPolyNeq(None)),$1),$3) }
  | sep_expr SHIFTLL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cslli(None)),$1),$3) }
  | sep_expr SHIFTRL expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Csrli(None)),$1),$3) }
  | sep_expr SHIFTRA expr
      { TmApp($2.i,TmApp($2.i,TmConst($2.i,Csrai(None)),$1),$3) }
  | NOT expr
      { TmApp($1.i,TmConst($1.i,Cnot),$2) }
  | sep_expr AND expr
            { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cand(None)),$1),$3) }
  | sep_expr OR expr
             { TmApp($2.i,TmApp($2.i,TmConst($2.i,Cor(None)),$1),$3) }
  | sep_expr CONCAT expr
         { TmApp($2.i,TmApp($2.i,TmConst($2.i,CConcat(None)),$1),$3) }
  | expr_aux { $1 }

expr_aux:
  | LPAREN expr RPAREN   { $2 }
  | LSQUARE exprs RSQUARE { TmUC($1.i,UCLeaf($2),UCOrdered,UCMultivalued) }
  | LCURLY top_treeppl_scope RCURLY  { $2 }
  | IDENT                { TmVar($1.i,$1.v,noidx,false) }
  | CHAR                 { TmChar($1.i, List.hd (ustring2list $1.v)) }
  | STRING               { ustring2uctm $1.i $1.v }
  | UINT                 { TmConst($1.i, CInt($1.v)) }
  | UFLOAT               { TmConst($1.i, CFloat($1.v)) }
  | TRUE                 { TmConst($1.i, CBool(true)) }
  | FALSE                { TmConst($1.i, CBool(false)) }

params:
  | { [] }
  | paramlist { $1 }

paramlist:
  | IDENT { [$1.v] }
  | IDENT COMMA paramlist { $1.v :: $3 }

exprs:
  | { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

