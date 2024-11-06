include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "mexpr/eq.mc"
include "mexpr/type-check.mc"
include "mexpr/anf.mc"
include "mexpr/cps.mc"
include "mexpr/type-lift.mc"

include "../coreppl.mc"

-- Explicit resample inference annotation for SMC
lang Resample = Ast + PrettyPrint + Eq + Sym + ANF + TypeLift + TypeCheck

  syn Expr =
  | TmResample { ty: Type, info: Info }

  sem infoTm =
  | TmResample t -> t.info

  sem tyTm =
  | TmResample t -> t.ty

  sem withInfo (info: Info) =
  | TmResample t -> TmResample { t with info = info }

  sem withType (ty: Type) =
  | TmResample t -> TmResample { t with ty = ty }

  sem smapAccumL_Expr_Expr f acc =
  | TmResample t -> (acc,TmResample t)

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

  -- Type check
  sem typeCheckExpr (env : TCEnv) =
  | TmResample t -> TmResample { t with ty = tyWithInfo t.info tyunit_ }

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
  TmResample { ty = tyunit_, info = NoInfo () }

----------------------
-- COMMON FUNCTIONS --
----------------------

lang SMCCommon = MExprPPL + Resample + MExprCPS

  -- Add resample after weights (given a predicate over identifiers, assumes
  -- ANF). Used in SMC compilers for both the RootPPL and MExpr backends.
  sem addResample: (Name -> Bool) -> Expr -> Expr
  sem addResample pred =
    | t ->
      let t = smap_Expr_Expr (addResample pred) t in
      match t
      with TmLet ({ ident = ident, body = TmWeight _, inexpr = inexpr } & r)
         | TmLet ({ ident = ident, body = TmObserve _, inexpr = inexpr } & r)
      then
        if pred ident then
          let resample = withInfo r.info resample_ in
          let l = nlet_ (nameSym "resample") tyunit_ resample in
          let l = withInfo r.info l in
          let inexpr = bind_ l inexpr in
          TmLet { r with inexpr = inexpr }
        else t
      else t

  -- CPS compile
  sem exprCps env k =
  | TmLet ({ body = TmAssume _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmObserve _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmWeight _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet ({ body = TmDist _ } & t) ->
    TmLet { t with inexpr = exprCps env k t.inexpr }
  | TmLet { ident = ident, body = TmResample {},
            inexpr = inexpr } & t ->
    let i = withInfo (infoTm t) in
    let k =
      if tailCall t then
        match k with Some k then
          k
        else
          error "Something went wrong with partial CPS transformation"
      else
        i (nulam_ ident (exprCps env k inexpr))
    in
      i (appf1_ (i (var_ "resample")) k)

  -- NOTE(2023-08-08,dlunde): Many TmTypes are shared with non-PPL code and
  -- transformed versions are removed when removing duplicate code.
  -- Therefore, we have to simply replace TyCon and TyApp with Unknown here.
  sem tyCps env =
  | (TyCon { info = info } | TyApp { info = info } ) ->
    let i = tyWithInfo info in i tyunknown_

  sem transformProb =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (var_ "sample")) t.dist)
  | TmResample t -> errorSingle [t.info] "Impossible"
  | TmObserve t ->
    let i = withInfo t.info in
    let weight = i (appf2_ (i (var_ "logObserve")) t.dist t.value) in
    i (appf2_ (i (var_ "updateWeight")) weight (i (var_ "state")))
  | TmWeight t ->
    let i = withInfo t.info in
    i (appf2_ (i (var_ "updateWeight")) t.weight (i (var_ "state")))
  | t -> t
end

-----------------------
-- LANGUAGE FRAGMENT --
-----------------------

lang SMC = Resample
end

lang Test =
  Resample + MExprEq + MExprSym + MExprTypeCheck + MExprANF
  + MExprTypeLift + MExprPrettyPrint
end

mexpr

use Test in


------------------------
-- PRETTY-PRINT TESTS --
------------------------

utest mexprToString resample_
with strJoin "\n" [
  "resample"
] using eqString in


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

utest sfold_Expr_Expr foldToSeq [] resample_ with [] using eqSeq eqExpr in


---------------------
-- SYMBOLIZE TESTS --
---------------------

utest symbolize resample_ with resample_ using eqExpr in


-------------------------
-- TYPE-CHECK TESTS --
-------------------------

utest tyTm (typeCheck resample_) with tyunit_ using eqType in

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

