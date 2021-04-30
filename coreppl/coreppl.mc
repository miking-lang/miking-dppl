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
include "dist.mc"
include "string.mc"



lang Infer = Ast + PrettyPrint + Eq + Sym

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

end


-- Assume defines a new random variable
lang Assume = Ast + Dist + PrettyPrint + Eq + Sym

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

end


-- Observe gives a random variable conditioned on a specific value
lang Observe = Ast + Dist + PrettyPrint + Eq + Sym

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

end

-- Defines a weight term
lang Weight = Ast + PrettyPrint + Eq + Sym
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

lang CorePPLInference = CorePPL -- + Importance + SMC

lang MExprPPL =
  CorePPLInference + MExprAst + MExprPrettyPrint + MExprEq + MExprSym


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
  "  (Bern",
  "     7.0e-1)"
] in

utest expr2str tmObserve
with strJoin "\n" [
  "observe",
  "  1.50e+0",
  "  (Beta",
  "     1.0e-0",
  "     2.0e+0)"
] in

utest expr2str tmWeight
with strJoin "\n" [
  "weight",
  "  1.50e+0"
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

()

