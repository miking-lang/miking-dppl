

include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/info.mc"
include "mexpr/eq.mc"
include "mexpr/type-annot.mc"
include "mexpr/anf.mc"

include "string.mc"
include "seq.mc"

lang Dist = PrettyPrint + Eq + Sym + TypeAnnot + ANF
  syn Expr =
  | TmDist { dist: Dist,
             ty: Type,
             info: Info }

  syn Type =
  | TyDist {info : Info,
            ty   : Type}

  syn Dist =
  -- Intentionally left blank

  sem infoTm =
  | TmDist t -> t.info

  sem ty =
  | TmDist t -> t.ty

  sem withType (ty: Type) =
  | TmDist t -> TmDist { t with ty = ty }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  -- Intentionally left blank

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  -- Intentionally left blank

  sem smap_Expr_Expr (f: Expr -> a) =
  | TmDist t -> TmDist { t with dist = smapDist_Expr_Expr f t.dist }

  sem sfold_Expr_Expr (f: a -> b -> a) (acc: a) =
  | TmDist t -> sfoldDist_Expr_Expr f acc t.dist

  -- Pretty printing
  sem isAtomic =
  | TmDist _ -> false

  sem pprintDist (indent: Int) (env: PprintEnv) =
  -- Intentionally left blank

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmDist t -> pprintDist indent env t.dist

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  -- Intentionally left blank

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmDist r ->
    match lhs with TmDist l then eqExprHDist env free l.dist r.dist else None ()

  sem eqType (typeEnv : EqTypeEnv) (lhs : Type) =
  | TyDist r ->
    match unwrapType typeEnv lhs with Some (TyDist l) then
      eqType typeEnv l.ty r.ty
    else false

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  -- Intentionally left blank

  sem symbolizeExpr (env: SymEnv) =
  | TmDist t ->
    TmDist {{ t with dist = symbolizeDist env t.dist }
                with ty = symbolizeType env t.ty }

  -- Type annotate
  sem compatibleTypeBase (tyEnv: TypeEnv) =
  | (TyDist t1, TyDist t2) ->
    match compatibleType tyEnv t1.ty t2.ty with Some t then
      Some (TyDist {t1 with ty = t})
    else None ()

  sem tyDist (env: TypeEnv) (info: Info) =
  -- Intentionally left blank

  sem typeAnnotDist (env: TypeEnv) =
  | dist -> smapDist_Expr_Expr (typeAnnotExpr env) dist

  sem typeAnnotExpr (env: TypeEnv) =
  | TmDist t ->
    let dist = typeAnnotDist env t.dist in
    let ty = TyDist { info = t.info, ty = tyDist env t.info dist } in
    TmDist {{ t with dist = dist }
                with ty = ty }

  -- ANF
  sem isValueDist =
  -- Intentionally left blank

  sem normalizeDist (k : Dist -> Expr) =
  -- Intentionally left blank

  sem isValue =
  | TmDist t -> isValueDist t.dist

  sem normalize (k : Expr -> Expr) =
  | TmDist ({ dist = dist } & t) ->
    normalizeDist (lam dist. k (TmDist { t with dist = dist })) dist

end

lang BernDist = Dist + PrettyPrint + Eq + Sym + BoolTypeAst + FloatTypeAst

  syn Dist =
  | DBern { p: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DBern t -> DBern { t with p = f t.p }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DBern t -> f acc t.p

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBern t ->
    let i = pprintIncr indent in
    match printParen i env t.p with (env,p) then
      (env, join ["Bern", pprintNewline i, p])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DBern r ->
    match lhs with DBern l then eqExprH env free l.p r.p else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DBern t -> DBern { t with p = symbolizeExpr env t.p }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DBern t ->
    match ty t.p with TyFloat _ then TyBool { info = NoInfo () }
    else infoErrorExit info "Type error"

  -- ANF
  sem isValueDist =
  | DBern _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DBern ({ p = p } & t) ->
    normalizeName (lam p. k (DBern { t with p = p })) p

end

lang BetaDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DBeta { a: Expr, b: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DBeta t -> DBeta {{ t with a = f t.a }
                          with b = f t.b }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DBeta t -> f (f acc t.a) t.b

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DBeta t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a, t.b] with (env,args) then
      (env, join ["Beta", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DBeta r ->
    match lhs with DBeta l then
      match eqExprH env free l.a r.a with Some free then
        eqExprH env free l.b r.b
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DBeta t -> DBeta {{ t with a = symbolizeExpr env t.a }
                          with b = symbolizeExpr env t.b }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DBeta t ->
    let err = lam. infoErrorExit info "Type error" in
    match ty t.a with TyFloat _ then
      match ty t.b with TyFloat _ then
        TyFloat { info = NoInfo () }
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DBeta _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DBeta ({ a = a, b = b } & t) ->
    normalizeName (lam a.
      normalizeName (lam b.
        k (DBeta {{ t with a = a } with b = b})) b) a

end

-- DCategorical {p=p} is equivalent to DMultinomial {n=1, p=p}
lang CategoricalDist =
  Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- p has type [Float]: the list of probabilities
  | DCategorical { p: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DCategorical t -> DCategorical { t with p = f t.p }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DCategorical t -> f acc t.p

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DCategorical t ->
    let i = pprintIncr indent in
    match printArgs i env [t.p] with (env,p) then
      (env, join ["Categorical", pprintNewline i, p])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DCategorical { p = p2 } ->
    match lhs with DCategorical { p = p1 } then
      eqExprH env free p1 p2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DCategorical t ->
    DCategorical { t with p = symbolizeExpr env t.p }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DCategorical t ->
    match ty t.p with TySeq { ty = TyFloat _ } then TyInt { info = NoInfo () }
    else infoErrorExit info "Type error"

  -- ANF
  sem isValueDist =
  | DCategorical _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DCategorical ({ p = p } & t) ->
    normalizeName (lam p. k (DCategorical {t with p = p})) p

end

lang MultinomialDist =
  Dist + PrettyPrint + Eq + Sym + IntTypeAst + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- n has type Int: the number of trials
  -- p has type [Float]: the list of probabilities
  | DMultinomial { n: Expr, p: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DMultinomial t -> DMultinomial {{ t with n = f t.n }
                                        with p = f t.p }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DMultinomial t -> f (f acc t.n) t.p

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DMultinomial t ->
    let i = pprintIncr indent in
    match printArgs i env [t.n, t.p] with (env,args) then
      (env, join ["Multinomial", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DMultinomial { n = n2, p = p2 } ->
    match lhs with DMultinomial { n = n1, p = p1 } then
      match eqExprH env free n1 n2 with Some free then
        eqExprH env free p1 p2
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DMultinomial t ->
    DMultinomial {{ t with n = symbolizeExpr env t.n }
                      with p = symbolizeExpr env t.p }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DMultinomial t ->
    let err = lam. infoErrorExit info "Type error" in
    match ty t.n with TyInt _ then
      match ty t.p with TySeq { ty = TyFloat _ } then
        TyInt { info = NoInfo () }
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DMultinomial _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DMultinomial ({ n = n, p = p } & t) ->
    normalizeName (lam n.
      normalizeName (lam p.
        k (DMultinomial {{ t with n = n } with p = p})) p) n

end

lang DirichletDist = Dist + PrettyPrint + Eq + Sym + SeqTypeAst + FloatTypeAst

  syn Dist =
  -- a has type [Float]: the list of concentration parameters
  | DDirichlet { a: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DDirichlet t -> DDirichlet { t with a = f t.a }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DDirichlet t -> f acc t.a

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DDirichlet t ->
    let i = pprintIncr indent in
    match printArgs i env [t.a] with (env,a) then
      (env, join ["Dirichlet", pprintNewline i, a])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DDirichlet { a = a2 } ->
    match lhs with DDirichlet { a = a1 } then
      eqExprH env free a1 a2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DDirichlet t ->
    DDirichlet { t with a = symbolizeExpr env t.a }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DDirichlet t ->
    match ty t.a with TySeq { ty = TyFloat _ } then
      TySeq { info = NoInfo (), ty = TyFloat { info = NoInfo () } }
    else infoErrorExit info "Type error"

  -- ANF
  sem isValueDist =
  | DDirichlet _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DDirichlet ({ a = a } & t) ->
    normalizeName (lam a. k (DDirichlet { t with a = a })) a

end

lang ExpDist = Dist + PrettyPrint + Eq + Sym + FloatTypeAst

  syn Dist =
  | DExp { rate: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DExp t -> DExp { t with rate = f t.rate }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DExp t -> f acc t.rate

  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DExp t ->
    let i = pprintIncr indent in
    match printParen i env t.rate with (env,rate) then
      (env, join ["Exp", pprintNewline i, rate])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DExp r ->
    match lhs with DExp l then eqExprH env free l.rate r.rate else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DExp t -> DExp { t with rate = symbolizeExpr env t.rate }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DExp t ->
    match ty t.rate with TyFloat _ then TyFloat { info = NoInfo () }
    else infoErrorExit info "Type error"

  -- ANF
  sem isValueDist =
  | DExp _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DExp ({ rate = rate } & t) ->
    normalizeName (lam rate. k (DExp { t with rate = rate })) rate

end

lang EmpiricalDist =
  Dist + PrettyPrint + Eq + Sym + FloatTypeAst + RecordTypeAst + SeqTypeAst

  syn Dist =
  -- samples has type [(Float,a)]: A set of weighted samples over type a
  | DEmpirical { samples: Expr }

  sem smapDist_Expr_Expr (f: Expr -> a) =
  | DEmpirical t -> DEmpirical { t with samples = f t.samples }

  sem sfoldDist_Expr_Expr (f: a -> b -> a) (acc: a) =
  | DEmpirical t -> f acc t.samples

  -- Pretty printing
  sem pprintDist (indent: Int) (env: PprintEnv) =
  | DEmpirical t ->
    let i = pprintIncr indent in
    match printParen i env t.samples with (env,samples) then
      (env, join ["Empirical", pprintNewline i, samples])
    else never

  -- Equality
  sem eqExprHDist (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | DEmpirical { samples = s2 } ->
    match lhs with DEmpirical { samples = s1 } then
      eqExprH env free s1 s2
    else None ()

  -- Symbolize
  sem symbolizeDist (env: SymEnv) =
  | DEmpirical t ->
    DEmpirical { t with samples = symbolizeExpr env t.samples }

  -- Type Annotate
  sem tyDist (env: TypeEnv) (info: Info) =
  | DEmpirical t ->
    let err = lam. infoErrorExit info "Type error" in
    match ty t.samples
    with TySeq { ty = TyRecord { fields = fields } } then
      if eqi (mapSize fields) 2 then
        match mapLookup (stringToSid "0") fields with Some TyFloat _ then
          match mapLookup (stringToSid "1") fields with Some ty then
            ty
          else err ()
        else err ()
      else err ()
    else err ()

  -- ANF
  sem isValueDist =
  | DEmpirical _ -> false

  sem normalizeDist (k : Dist -> Expr) =
  | DEmpirical ({ samples = samples } & t) ->
      normalizeName
        (lam samples. k (DEmpirical { t with samples = samples })) samples

end

-----------------
-- AST BUILDER --
-----------------

let dist_ = use Dist in
  lam d. TmDist {dist = d, ty = tyunknown_, info = NoInfo ()}

let tydist_ = use Dist in
  lam ty. TyDist {info = NoInfo (), ty = ty}

let bern_ = use BernDist in
  lam p. dist_ (DBern {p = p})

let beta_ = use BetaDist in
  lam a. lam b. dist_ (DBeta {a = a, b = b})

let categorical_ = use CategoricalDist in
  lam p. dist_ (DCategorical {p = p})

let multinomial_ = use MultinomialDist in
  lam n. lam p. dist_ (DMultinomial {n = n, p = p})

let dirichlet_ = use DirichletDist in
  lam a. dist_ (DDirichlet {a = a})

let exp_ = use ExpDist in
  lam rate. dist_ (DExp {rate = rate})

let empirical_ = use EmpiricalDist in
  lam lst. dist_ (DEmpirical {samples = lst})


---------------------------
-- LANGUAGE COMPOSITIONS --
---------------------------

lang DistAll =
  BernDist + BetaDist + ExpDist + EmpiricalDist + CategoricalDist +
  MultinomialDist + DirichletDist

lang Test =
  DistAll + MExprAst + MExprPrettyPrint + MExprEq + MExprSym + MExprTypeAnnot
  + MExprANF

mexpr


use Test in

let tmBern = bern_ (float_ 0.5) in
let tmBeta = beta_ (float_ 1.0) (float_ 2.0) in
let tmCategorical =
  categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]) in
let tmMultinomial =
  multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]) in
let tmExp = exp_ (float_ 1.0) in
let tmEmpirical = empirical_ (seq_ [
    tuple_ [float_ 1.0, float_ 1.5],
    tuple_ [float_ 3.0, float_ 1.3]
  ]) in
let tmDirichlet = dirichlet_ (seq_ [float_ 1.3, float_ 1.3, float_ 1.5]) in

------------------------
-- PRETTY-PRINT TESTS --
------------------------

utest expr2str tmBern with strJoin "\n" [
  "Bern",
  "  5.0e-1"
] in

utest expr2str tmBeta with strJoin "\n" [
  "Beta",
  "  1.0e-0",
  "  2.0e+0"
] in

utest expr2str tmCategorical with strJoin "\n" [
  "Categorical",
  "  [ 3.0e-1,",
  "    2.0e-1,",
  "    5.0e-1 ]"
] in

utest expr2str tmMultinomial with strJoin "\n" [
  "Multinomial",
  "  5",
  "  [ 3.0e-1,",
  "    2.0e-1,",
  "    5.0e-1 ]"
] in

utest expr2str tmExp with strJoin "\n" [
  "Exp",
  "  1.0e-0"
] in

utest expr2str tmEmpirical with strJoin "\n" [
  "Empirical",
  "  [ (1.0e-0, 1.50e+0),",
  "    (3.0e+0, 1.300000e+0) ]"
] in

utest expr2str tmDirichlet with strJoin "\n" [
  "Dirichlet",
  "  [ 1.300000e+0,",
  "    1.300000e+0,",
  "    1.50e+0 ]"
] in


--------------------
-- EQUALITY TESTS --
--------------------

utest tmBern with tmBern using eqExpr in
utest eqExpr tmBern (bern_ (float_ 0.4)) with false in

utest tmBeta with tmBeta using eqExpr in
utest eqExpr tmBeta (beta_ (float_ 1.0) (float_ 1.0)) with false in

utest tmCategorical with tmCategorical using eqExpr in
utest eqExpr tmCategorical
  (categorical_ (seq_ [float_ 0.2, float_ 0.2, float_ 0.5])) with false in
utest eqExpr tmCategorical
  (categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.6])) with false in

utest tmMultinomial with tmMultinomial using eqExpr in
utest eqExpr tmMultinomial
  (multinomial_ (int_ 4) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]))
with false in
utest eqExpr tmMultinomial
  (multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.3, float_ 0.5]))
with false in

utest tmExp with tmExp using eqExpr in
utest eqExpr tmExp (exp_ (float_ 1.1)) with false in

utest tmEmpirical with tmEmpirical using eqExpr in
utest eqExpr tmEmpirical (empirical_ (seq_ [
    tuple_ [float_ 2.0, float_ 1.5],
    tuple_ [float_ 3.0, float_ 1.3]
  ]))
with false in
utest eqExpr tmEmpirical (empirical_ (seq_ [
    tuple_ [float_ 2.0, float_ 1.5],
    tuple_ [float_ 3.0, float_ 1.4]
  ]))
with false in

utest tmDirichlet with tmDirichlet using eqExpr in
utest eqExpr tmDirichlet
  (dirichlet_ (seq_ [float_ 1.2, float_ 0.5, float_ 1.5])) with false in

----------------------
-- SMAP/SFOLD TESTS --
----------------------

let tmVar = var_ "x" in
let mapVar = (lam. tmVar) in
let foldToSeq = lam a. lam e. cons e a in

utest smap_Expr_Expr mapVar tmBern with bern_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBern
with [ float_ 0.5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmBeta with beta_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmBeta
with [ float_ 2.0, float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmCategorical with categorical_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmCategorical
with [ seq_ [float_ 0.3, float_ 0.2, float_ 0.5] ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmMultinomial
with multinomial_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmMultinomial
with [ seq_ [float_ 0.3, float_ 0.2, float_ 0.5], int_ 5 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmExp with exp_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmExp
with [ float_ 1.0 ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmEmpirical with empirical_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmEmpirical
with [
  seq_ [ tuple_ [float_ 1.0, float_ 1.5], tuple_ [float_ 3.0, float_ 1.3] ]
] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmDirichlet with dirichlet_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmDirichlet
with [ seq_ [float_ 1.3, float_ 1.3, float_ 1.5] ] using eqSeq eqExpr in


---------------------
-- SYMBOLIZE TESTS --
---------------------

utest symbolize tmBern with tmBern using eqExpr in
utest symbolize tmBeta with tmBeta using eqExpr in
utest symbolize tmCategorical with tmCategorical using eqExpr in
utest symbolize tmMultinomial with tmMultinomial using eqExpr in
utest symbolize tmExp with tmExp using eqExpr in
utest symbolize tmEmpirical with tmEmpirical using eqExpr in
utest symbolize tmDirichlet with tmDirichlet using eqExpr in


-------------------------
-- TYPE-ANNOTATE TESTS --
-------------------------

let eqTypeEmptyEnv : Type -> Type -> Bool = eqType [] in

utest ty (typeAnnot tmBern) with tydist_ tybool_ using eqTypeEmptyEnv in
utest ty (typeAnnot tmBeta) with tydist_ tyfloat_ using eqTypeEmptyEnv in
utest ty (typeAnnot tmCategorical) with tydist_ tyint_ using eqTypeEmptyEnv in
utest ty (typeAnnot tmMultinomial) with tydist_ tyint_ using eqTypeEmptyEnv in
utest ty (typeAnnot tmExp) with tydist_ tyfloat_ using eqTypeEmptyEnv in
utest ty (typeAnnot tmEmpirical) with tydist_ tyfloat_ using eqTypeEmptyEnv in
utest ty (typeAnnot tmDirichlet) with tydist_ (tyseq_ tyfloat_)
using eqTypeEmptyEnv in

---------------
-- ANF TESTS --
---------------

let _anf = compose normalizeTerm symbolize in

utest _anf tmBern with bind_ (ulet_ "t" tmBern) (var_ "t") using eqExpr in
utest _anf tmBeta with bind_ (ulet_ "t" tmBeta) (var_ "t") using eqExpr in
utest _anf tmCategorical with bindall_ [
  ulet_ "t" (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]),
  ulet_ "t1" (categorical_ (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmMultinomial with bindall_ [
  ulet_ "t" (seq_ [float_ 0.3, float_ 0.2, float_ 0.5]),
  ulet_ "t1" (multinomial_ (int_ 5) (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmExp with bind_ (ulet_ "t" tmExp) (var_ "t") using eqExpr in
utest _anf tmEmpirical with bindall_ [
  ulet_ "t" (tuple_ [float_ 3.0, float_ 1.3]),
  ulet_ "t1" (tuple_ [float_ 1.0, float_ 1.5]),
  ulet_ "t2" (seq_ [(var_ "t1"), (var_ "t")]),
  ulet_ "t3" (empirical_ (var_ "t2")),
  var_ "t3"
] using eqExpr in
-- print (expr2str (_anf tmEmpirical)); print "\n";
utest _anf tmDirichlet with bindall_ [
  ulet_ "t" (seq_ [float_ 1.3, float_ 1.3, float_ 1.5]),
  ulet_ "t1" (dirichlet_ (var_ "t")),
  var_ "t1"
] using eqExpr in

()

