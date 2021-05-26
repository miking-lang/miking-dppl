-- CorePPL
-- Note that we should NOT implement eval or compile functions
-- CorePPL. Instead, we implement function 'toMExpr' which translates
-- the core terms into an MExpr term. By doing so, we can use the
-- standard eval and compile functions when running the inference.

include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/mexpr.mc"
include "mexpr/info.mc"
include "mexpr/eq.mc"
include "mexpr/anf.mc"
include "mexpr/type-annot.mc"
include "mexpr/type-lift.mc"

include "string.mc"

include "dist.mc"
include "smc.mc"

-------------
-- HELPERS --
-------------

let _isUnitTy = use RecordTypeAst in lam ty.
  match ty with TyRecord { fields = fields } then mapIsEmpty fields
  else false


-----------
-- TERMS --
-----------

lang Infer =
  Ast + PrettyPrint + Eq + Sym + Dist + FunTypeAst + TypeAnnot + ANF + TypeLift

  -- Evaluation of TmInfer returns a TmDist
  syn Expr =
  | TmInfer { method: InferMethod,
              model: Expr,
              ty: Type,
              info: Info }

  -- Interface type for infer methods
  syn InferMethod =

  sem infoTm =
  | TmInfer t -> t.info

  sem ty =
  | TmInfer t -> t.ty

  sem withType (ty: Type) =
  | TmInfer t -> TmInfer { t with ty = ty }

  sem smap_Expr_Expr (f: Expr -> a) =
  | TmInfer t -> TmInfer { t with model = f t.model }

  sem sfold_Expr_Expr (f: a -> b -> a) (acc: a) =
  | TmInfer t -> f acc t.model

  -- Pretty printing
  sem isAtomic =
  | TmInfer _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmInfer t ->
    -- TODO(dlunde,2021-04-28): Infer method not yet printed
    let i = pprintIncr indent in
    match printParen i env t.model with (env,model) then
      (env, join ["infer", pprintNewline i, model])
    else never

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmInfer r ->
    -- TODO(dlunde,2021-04-28): Infer method not yet checked for equality
    match lhs with TmInfer l then
      eqExprH env free l.model r.model
    else None ()

  -- Symbolize
  sem symbolizeExpr (env: SymEnv) =
  | TmInfer t ->
    TmInfer {{ t with model = symbolizeExpr env t.model }
                 with ty = symbolizeType env t.ty }

  -- Type annotate
  sem typeAnnotExpr (env: TypeEnv) =
  | TmInfer t ->
    let err = lam. infoErrorExit t.info "Type error infer" in
    let model = typeAnnotExpr env t.model in
    let ty =
      match ty model with TyArrow { from = from, to = to } then
        if _isUnitTy from then to
        else err ()
      else err ()
    in
    TmInfer {{ t with model = model }
                 with ty = TyDist { info = t.info, ty = ty } }

  -- ANF
  sem isValue =
  | TmInfer _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmInfer ({ model = model } & t) ->
    normalizeName (lam model. k (TmInfer { t with model = model })) model

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmInfer t ->
    match typeLiftExpr env t.model with (env, model) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmInfer {{t with model = model}
                          with ty = ty})
      else never
    else never

end


-- Assume defines a new random variable
lang Assume = Ast + Dist + PrettyPrint + Eq + Sym + TypeAnnot + ANF + TypeLift

  syn Expr =
  | TmAssume { dist: Expr,
               ty: Type,
               info: Info }

  sem infoTm =
  | TmAssume t -> t.info

  sem ty =
  | TmAssume t -> t.ty

  sem withType (ty: Type) =
  | TmAssume t -> TmAssume { t with ty = ty }

  sem smap_Expr_Expr (f: Expr -> a) =
  | TmAssume t -> TmAssume { t with dist = f t.dist }

  sem sfold_Expr_Expr (f: a -> b -> a) (acc: a) =
  | TmAssume t -> f acc t.dist

  -- Pretty printing
  sem isAtomic =
  | TmAssume _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmAssume t ->
    let i = pprintIncr indent in
    match printParen i env t.dist with (env,dist) then
      (env, join ["assume", pprintNewline i, dist])
    else never

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmAssume r ->
    match lhs with TmAssume l then
      eqExprH env free l.dist r.dist
    else None ()

  -- Symbolize
  sem symbolizeExpr (env: SymEnv) =
  | TmAssume t ->
    TmAssume {{ t with dist = symbolizeExpr env t.dist }
                  with ty = symbolizeType env t.ty }

  -- Type annotate
  sem typeAnnotExpr (env: TypeEnv) =
  | TmAssume t ->
    let err = lam. infoErrorExit t.info "Type error assume" in
    let dist = typeAnnotExpr env t.dist in
    let ty =
      match ty dist with TyDist { ty = ty } then ty
      else err ()
    in
    TmAssume {{ t with dist = dist }
                  with ty = ty }

  -- ANF
  sem isValue =
  | TmAssume _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmAssume ({ dist = dist } & t) ->
    normalizeName (lam dist. k (TmAssume { t with dist = dist })) dist

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmAssume t ->
    match typeLiftExpr env t.dist with (env, dist) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmAssume {{t with dist = dist}
                           with ty = ty})
      else never
    else never

end


-- Observe gives a random variable conditioned on a specific value
lang Observe = Ast + Dist + PrettyPrint + Eq + Sym + TypeAnnot + ANF + TypeLift

  syn Expr =
  | TmObserve { value: Expr,
                dist: Expr,
                ty: Type,
                info: Info }

  sem infoTm =
  | TmObserve t -> t.info

  sem ty =
  | TmObserve t -> t.ty

  sem withType (ty: Type) =
  | TmObserve t -> TmObserve { t with ty = ty }

  sem smap_Expr_Expr (f: Expr -> a) =
  | TmObserve t -> TmObserve {{ t with value = f t.value }
                                  with dist = f t.dist }

  sem sfold_Expr_Expr (f: a -> b -> a) (acc: a) =
  | TmObserve t -> f (f acc t.value) t.dist

  -- Pretty printing
  sem isAtomic =
  | TmObserve _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmObserve t ->
    let i = pprintIncr indent in
    match printArgs i env [t.value, t.dist] with (env,args) then
      (env, join ["observe", pprintNewline i, args])
    else never

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmObserve r ->
    match lhs with TmObserve l then
      match eqExprH env free l.value r.value with Some free then
        eqExprH env free l.dist r.dist
      else None ()
    else None ()

  -- Symbolize
  sem symbolizeExpr (env: SymEnv) =
  | TmObserve t ->
    TmObserve {{{ t with value = symbolizeExpr env t.value }
                    with dist = symbolizeExpr env t.dist }
                    with ty = symbolizeType env t.ty }

  -- Type annotate
  sem typeAnnotExpr (env: TypeEnv) =
  | TmObserve t ->
    let err = lam. infoErrorExit t.info "Type error observe" in
    let value = typeAnnotExpr env t.value in
    let dist = typeAnnotExpr env t.dist in
    let ty =
      match ty value with ty1 then
        match ty dist with TyDist { ty = ty2 } then
          match compatibleType env ty1 ty2 with Some _ then
            tyunit_
          else err ()
        else err ()
      else err ()
    in
    TmObserve {{{ t with value = value }
                    with dist = dist }
                    with ty = ty }

  -- ANF
  sem isValue =
  | TmObserve _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmObserve ({ value = value, dist = dist } & t) ->
    normalizeName
      (lam value.
        normalizeName
          (lam dist.
            k (TmObserve {{ t with value = value }
                              with dist = dist }))
        dist)
      value

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmObserve t ->
    match typeLiftExpr env t.value with (env, value) then
      match typeLiftExpr env t.dist with (env, dist) then
        match typeLiftType env t.ty with (env, ty) then
          (env, TmObserve {{{ t with value = value }
                                with dist = dist }
                                with ty = ty })
        else never
      else never
    else never

end

-- Defines a weight term
lang Weight =
  Ast + PrettyPrint + Eq + Sym + TypeAnnot + FloatTypeAst + ANF + TypeLift
  syn Expr =
  | TmWeight { weight: Expr, ty: Type, info: Info }

  sem infoTm =
  | TmWeight t -> t.info

  sem ty =
  | TmWeight t -> t.ty

  sem withType (ty: Type) =
  | TmWeight t -> TmWeight { t with ty = ty }

  sem smap_Expr_Expr (f: Expr -> a) =
  | TmWeight t -> TmWeight { t with weight = f t.weight }

  sem sfold_Expr_Expr (f: a -> b -> a) (acc: a) =
  | TmWeight t -> f acc t.weight

  -- Pretty printing
  sem isAtomic =
  | TmWeight _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmWeight t ->
    let i = pprintIncr indent in
    match printParen i env t.weight with (env,dist) then
      (env, join ["weight", pprintNewline i, dist])
    else never

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmWeight r ->
    match lhs with TmWeight l then
      eqExprH env free l.weight r.weight
    else None ()

  -- Symbolize
  sem symbolizeExpr (env: SymEnv) =
  | TmWeight t ->
    TmWeight {{ t with weight = symbolizeExpr env t.weight }
                  with ty = symbolizeType env t.ty }

  -- Type annotate
  sem typeAnnotExpr (env: TypeEnv) =
  | TmWeight t ->
    let err = lam. infoErrorExit t.info "Type error weight" in
    let weight = typeAnnotExpr env t.weight in
    let ty =
      match ty weight with TyFloat _ then tyunit_
      else err ()
    in
    TmWeight {{ t with weight = weight }
                  with ty = ty }

  -- ANF
  sem isValue =
  | TmWeight _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmWeight ({ weight = weight } & t) ->
    normalizeName (lam weight. k (TmWeight { t with weight = weight })) weight

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmWeight t ->
    match typeLiftExpr env t.weight with (env, weight) then
      match typeLiftType env t.ty with (env, ty) then
        (env, TmWeight {{ t with weight = weight }
                            with ty = ty })
      else never
    else never

end

-- Translations in between weight and observe terms
lang ObserveWeightTranslation = Observe + Weight
/-
  -- Translates ALL observe terms into weight terms.
  sem observeToWeight =
  | TmObserve -> unit_ -- TODO

  -- Translates SOME weight terms into observe terms.
  sem weightToObserve =
  | TmWeight -> unit_ -- TODO
-/
end


-----------------
-- AST BUILDER --
-----------------

let infer_ = use Infer in
  lam d. lam m.
  TmInfer {method = d, model = m, ty = tyunknown_, info = NoInfo ()}

let assume_ = use Assume in
  lam d. TmAssume {dist = d, ty = tyunknown_, info = NoInfo ()}

let observe_ = use Observe in
  lam v. lam d.
  TmObserve {value = v, dist = d, ty = tyunknown_, info = NoInfo ()}

let weight_ = use Weight in
  lam w. TmWeight {weight = w, ty = tyunknown_, info = NoInfo ()}



---------------------------
-- LANGUAGE COMPOSITIONS --
---------------------------

lang CorePPL =
  Ast + Assume + Observe + Weight + ObserveWeightTranslation + DistAll

lang CorePPLInference = CorePPL + SMC -- + Importance

lang MExprPPL =
  CorePPLInference + MExprAst + MExprPrettyPrint + MExprEq + MExprSym +
  MExprTypeAnnot + MExprANF + MExprTypeLiftUnOrderedRecords +
  MExprPPLCmpTypeIndex


mexpr

use MExprPPL in

let tmAssume = assume_ (bern_ (float_ 0.7)) in
let tmObserve = observe_ (float_ 1.5) (beta_ (float_ 1.0) (float_ 2.0)) in
let tmWeight = weight_ (float_ 1.5) in

------------------------
-- PRETTY-PRINT TESTS --
------------------------
-- TODO(dlunde,2021-04-28): TmInfer test

utest expr2str tmAssume
with strJoin "\n" [
  "assume",
  "  (Bernoulli",
  "     0.7)"
] in

utest expr2str tmObserve
with strJoin "\n" [
  "observe",
  "  1.5",
  "  (Beta",
  "     1.",
  "     2.)"
] in

utest expr2str tmWeight
with strJoin "\n" [
  "weight",
  "  1.5"
] in

--------------------
-- EQUALITY TESTS --
--------------------
-- TODO(dlunde,2021-04-28): TmInfer test

utest tmAssume with tmAssume using eqExpr in
utest eqExpr tmAssume (assume_ (bern_ (float_ 0.6)))
with false in

utest tmObserve with tmObserve using eqExpr in
utest eqExpr tmObserve (observe_ (float_ 1.5) (beta_ (float_ 1.0) (float_ 2.1)))
with false in

utest tmWeight with tmWeight using eqExpr in
utest eqExpr tmWeight ((weight_ (float_ 1.6)))
with false in

----------------------
-- SMAP/SFOLD TESTS --
----------------------
-- TODO(dlunde,2021-04-28): TmInfer test

let tmVar = var_ "x" in
let mapVar = (lam. tmVar) in
let foldToSeq = lam a. lam e. cons e a in

utest smap_Expr_Expr mapVar tmAssume with assume_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmAssume
with [ bern_ (float_ 0.7) ] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmObserve with observe_ tmVar tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmObserve
with [
  (beta_ (float_ 1.0) (float_ 2.0)),
  (float_ 1.5)
] using eqSeq eqExpr in

utest smap_Expr_Expr mapVar tmWeight with weight_ tmVar using eqExpr in
utest sfold_Expr_Expr foldToSeq [] tmWeight
with [ float_ 1.5 ] using eqSeq eqExpr in

---------------------
-- SYMBOLIZE TESTS --
---------------------
-- TODO(dlunde,2021-04-28): TmInfer test

utest symbolize tmAssume with tmAssume using eqExpr in
utest symbolize tmObserve with tmObserve using eqExpr in
utest symbolize tmWeight with tmWeight using eqExpr in


-------------------------
-- TYPE-ANNOTATE TESTS --
-------------------------
-- TODO(dlunde,2021-04-28): TmInfer test

let _eqTypeEmptyEnv : Type -> Type -> Bool = eqType [] in

utest ty (typeAnnot tmAssume) with tybool_ using _eqTypeEmptyEnv in
utest ty (typeAnnot tmObserve) with tyunit_ using _eqTypeEmptyEnv in
utest ty (typeAnnot tmWeight) with tyunit_ using _eqTypeEmptyEnv in

---------------
-- ANF TESTS --
---------------
-- TODO(dlunde,2021-05-10): TmInfer test

let _anf = compose normalizeTerm symbolize in

utest _anf tmAssume with bindall_ [
  ulet_ "t" (bern_ (float_ 0.7)),
  ulet_ "t1" (assume_ (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmObserve with bindall_ [
  ulet_ "t" (beta_ (float_ 1.0) (float_ 2.0)),
  ulet_ "t1" (observe_ (float_ 1.5) (var_ "t")),
  var_ "t1"
] using eqExpr in
utest _anf tmWeight with bindall_ [
  ulet_ "t" (weight_ (float_ 1.5)),
  var_ "t"
] using eqExpr in

---------------------
-- TYPE-LIFT TESTS --
---------------------
-- TODO(dlunde,2021-05-10): TmInfer test

utest (typeLift tmAssume).1 with tmAssume using eqExpr in
utest (typeLift tmObserve).1 with tmObserve using eqExpr in
utest (typeLift tmWeight).1 with tmWeight using eqExpr in

()

