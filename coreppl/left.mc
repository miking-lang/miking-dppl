-- This file includes some of the things that will be removed, but are left here for now.

/-
   sem getConstStringCode (indent : Int) =
  | CWeight _ -> "weight"
  | CSample _ -> "sample"
  | CDist d -> join ["distribution", pprintNewline (pprintIncr indent), getDistStringCode indent d.dist]

  sem getDistStringCode (indent : Int) =
  | DExp d -> join ["exponential", pprintNewline (pprintIncr indent), d.a]
  | DBern d -> join ["bernoulli", pprintNewline (pprintIncr indent), d.p]
  | DBeta d -> join ["beta", pprintNewline (pprintIncr indent), d.a, " ", d.b]
-/



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
