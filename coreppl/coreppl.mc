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
include "dist.mc"
include "string.mc"
include "mikingmain.mc"



lang Infer = Ast
  -- Evaluation of TmInfer returns a TmDist
  syn Expr =
  | TmInfer { method: InferMethod,
              model: Expr}

  -- Interface type for infer methods
  syn InferMethod =

end


-- Assume defines a new random variable
lang Assume = Ast + Dist + PrettyPrint
  syn Expr =
  | TmAssume { dist: Expr,
               info: Info}

  sem isAtomic =
  | TmAssume _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmAssume r ->
    match pprintCode 0 env r.dist with (env,dist) then
      (env, join ["assume ", dist])
    else never

end


-- Observe gives a random variable conditioned on a specific value
lang Observe = Ast + Dist + PrettyPrint
  syn Expr =
  | TmObserve { value: Expr,
                dist: Expr,
                info: Info }

  sem isAtomic =
  | TmObserve _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmObserve r ->
    match pprintCode 0 env r.value with (env, value) then
      match pprintCode 0 env r.dist with (env, dist) then
        (env, join ["observe ", value, " ", dist])
      else never
    else never

end

-- Defines a weight term
lang Weight = Ast + PrettyPrint
  syn Expr =
  | TmWeight { weight: Expr }

  sem isAtomic =
  | TmWeight _ -> false

  sem pprintCode (indent : Int) (env: PprintEnv) =
  | TmWeight r ->
    match pprintCode 0 env r.weight with (env,weight) then
      (env, join ["weight ", weight])
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


-- Convenience functions for manually constructing ASTs

let assume_ = use Assume in
  lam d. TmAssume {dist = d, info = NoInfo ()}

let observe_ = use Observe in
  lam v. lam d. TmObserve {value = v, dist = d, info = NoInfo ()}

let weight_ = use Weight in
  lam w. TmWeight {weight = w, info = NoInfo ()}


-- Language compositions

lang CorePPL = Ast + Assume + Observe + Weight + ObserveWeightTranslation + DistAll

lang CorePPLinference = CorePPL -- + Importance + SMC

lang MExprPPL = CorePPLinference + MExpr




mexpr

use MExprPPL in

utest expr2str (assume_ (bern_ (float_ 0.7)))
  with "assume (Bern 7.0e-1)" in
utest expr2str (observe_ (float_ 1.5) (beta_ (float_ 1.0) (float_ 2.0)))
  with "observe 1.50e+0 (Beta 1.0e-0, 2.0e+0)" in
utest expr2str (weight_ (float_ 1.5))
  with "weight 1.50e+0" in
utest expr2str (assume_ (categorical_ (seq_ [float_ 0.3, float_ 0.2, float_ 0.5])))
  with "assume (Categorical [ 3.0e-1,\n  2.0e-1,\n  5.0e-1 ])" in
utest expr2str (assume_ (multinomial_ (int_ 5) (seq_ [float_ 0.3, float_ 0.2, float_ 0.5])))
  with "assume (Multinomial 5, [ 3.0e-1,\n  2.0e-1,\n  5.0e-1 ])" in
()

