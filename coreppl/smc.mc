-- CorePPL SMC
/-
lang CorePPLSMC = Infer + Ast

  syn InferMethod =
  | MethodSMC { particles: Expr }

  syn Const =
  | CResample {}


  -- Lower to an MExpr expression
  sem toMExpr =
  | TmAssume r -> unit_ -- TODO
  | TmObserve r -> unit_ -- TODO
  | TmWeight r -> unit_ -- TODO


  sem getConstStringCode (indent : Int) =
  | CResample _ -> "resample"

  sem getInferStringCode (indent : Int) =
  | MethodSMC m -> join ["smc" pprintNewline (pprintIncr indent), m.particles]

end

-- Convenience functions for manually constructing ASTs
let methodsmc_ = use CorePPLSMC in lam p. MethodSMC {particles = p}
-/

include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/eq.mc"
include "mexpr/type-annot.mc"
include "mexpr/anf.mc"
include "mexpr/type-lift.mc"

-- Explicit resample inference annotation for SMC
lang Resample = Ast + PrettyPrint + Eq + Sym + ANF + TypeLift

  syn Expr =
  | TmResample { ty: Type, info: Info }

  sem infoTm =
  | TmResample t -> t.info

  sem ty =
  | TmResample t -> t.ty

  sem withType (ty: Type) =
  | TmResample t -> TmResample { t with ty = ty }

  sem smap_Expr_Expr (f: Expr -> a) =
  | TmResample t -> TmResample t

  sem sfold_Expr_Expr (f: a -> b -> a) (acc: a) =
  | TmResample t -> acc

  -- Pretty printing
  sem isAtomic =
  | TmResample _ -> true

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmResample _ -> (env, "resample")

  -- Equality
  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmResample _ ->
    match lhs with TmResample _ then Some free else None ()

  -- Symbolize
  sem symbolizeExpr (env: SymEnv) =
  | TmResample t ->
    TmResample { t with ty = symbolizeType env t.ty }

  -- Type annotate
  sem typeAnnotExpr (env: TypeEnv) =
  | TmResample t -> TmResample { t with ty = tyunit_ }

  -- ANF
  sem isValue =
  | TmResample _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmResample t -> k (TmResample t)

  -- Type lift
  sem typeLiftExpr (env : TypeLiftEnv) =
  | TmResample t ->
    match typeLiftType env t.ty with (env, ty) then
      (env, TmResample { t with ty = ty })
    else never
end

-----------------
-- AST BUILDER --
-----------------

let resample_ = use Resample in
  TmResample { ty = tyunknown_, info = NoInfo () }

lang SMC = Resample

lang Test =
  Resample + MExprEq + MExprSym + MExprTypeAnnot + MExprANF
  + MExprTypeLiftUnOrderedRecordsCmpClosed

mexpr

use Test in


------------------------
-- PRETTY-PRINT TESTS --
------------------------

utest expr2str resample_
with strJoin "\n" [
  "resample"
] in


----------------------
-- EQUALITY TESTS --
----------------------

utest resample_ with resample_ using eqExpr in
utest eqExpr resample_ (var_ "x") with false in


----------------------
-- SMAP/SFOLD TESTS --
----------------------

let tmVar = var_ "x" in
let mapVar = (lam. tmVar) in
let foldToSeq = lam a. lam e. cons e a in

utest smap_Expr_Expr mapVar resample_ with resample_ using eqExpr in

utest sfold_Expr_Expr foldToSeq [] resample_ with [] in


---------------------
-- SYMBOLIZE TESTS --
---------------------

utest symbolize resample_ with resample_ using eqExpr in


-------------------------
-- TYPE-ANNOTATE TESTS --
-------------------------

let eqTypeEmptyEnv : Type -> Type -> Bool = eqType [] in

utest ty (typeAnnot resample_) with tyunit_ using eqTypeEmptyEnv in

---------------
-- ANF TESTS --
---------------

let _anf = compose normalizeTerm symbolize in

utest _anf resample_ with bindall_ [
  ulet_ "t" resample_,
  var_ "t"
] using eqExpr in

---------------------
-- TYPE-LIFT TESTS --
---------------------

utest (typeLift resample_).1 with resample_ using eqExpr in

()

