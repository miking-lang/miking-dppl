-- CorePPL

include "mexpr/ast.mc"
include "mexpr/pprint.mc"
-- include "mexpr/anf.mc"

-- anf.mc file content
--lang CorePPL = CorePPL + MExprANF
--
--  sem isValue =
--  | TmWeight _ -> false
--  | TmSampleExp _ -> false
--  | TmSampleBern _ -> false
--  | TmResample _ -> false
--
--  sem normalize (k : Expr -> Expr) =
--  | TmWeight ({ arg = arg } & t) ->
--    normalizeName (lam arg. k (TmWeight { t with arg = arg })) arg
--
--  | TmSampleExp ({ a = a } & t) ->
--    normalizeName (lam a. k (TmSampleExp { t with a = a })) a
--
--  | TmSampleBern ({ p = p } & t) ->
--    normalizeName (lam p. k (TmSampleBern { t with p = p })) p
--
--  | TmResample t -> k (TmResample t)
--end

lang Infer = MExprAst + MExprPrettyPrint

  syn Const =
  | CInfer { method: Option Const }
  | CInferMethod { method: InferMethod }

  syn InferMethod =

  sem getConstStringCode (indent : Int) =
  | CInfer c -> join ["infer", pprintNewline (pprintIncr indent), getConstStringCode indent c.method]
  | CInferMethod c ->  join ["infer method", pprintNewline (pprintIncr indent), getInferStringCode indent c.method]

  sem getInferStringCode (indent : Int) =

end

lang CorePPL = MExprAst + MExprPrettyPrint

  syn Const =
  | CWeight {}
  | CSample {}
  | CDist { dist: Dist }

  syn Dist =
  | DExp { a: Option Float }
  | DBern { p: Option Float }
  | DBeta { a: Option Float, b: Option Float }

   sem getConstStringCode (indent : Int) =
  | CWeight _ -> "weight"
  | CSample _ -> "sample"
  | CDist d -> join ["distribution", pprintNewline (pprintIncr indent), getDistStringCode indent d.dist]

  sem getDistStringCode (indent : Int) =
  | DExp d -> join ["exponential", pprintNewline (pprintIncr indent), d.a]
  | DBern d -> join ["bernoulli", pprintNewline (pprintIncr indent), d.p]
  | DBeta d -> join ["beta", pprintNewline (pprintIncr indent), d.a, " ", d.b]
end

lang Empirical = MExprPrettyPrint

  syn Dist =
  | DEmpirical { sample: [(Float, Expr)] }

  sem getConstStringCode (indent : Int) =
  | DEmpirical d ->
    let pprintEnvEmpty = { nameMap = mapEmpty nameCmp,
                          count = mapEmpty cmpString,
                          strings = mapEmpty cmpString } in
    join ["empirical", pprintNewline (pprintIncr indent), pprintCode indent pprintEnvEmpty d.sample]

end

-- Convenience functions for manually constructing ASTs

let infer_ = use Infer in lam m. CInfer {method = m}

let method_ = use Infer in lam m. CInferMethod {method = m}

let weight_ = use CorePPL in CWeight {}

let sample_ = use CorePPL in CSample {}

let cdist_ = use CorePPL in lam d. CDist {d = d}

let dexp_ = use CorePPL in lam a. DExp {a = a}

let dbern_ = use CorePPL in lam p. DBern {p = p}

let dbeta_ = use CorePPL in lam a. lam b. DBeta {a = a, b = b}

let dempirical_ = use Empirical in lam s. DEmpirical {sample = s}
