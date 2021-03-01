-- CorePPL

include "mexpr/ast.mc"
include "mexpr/anf.mc"
include "mexpr/eq.mc"
include "mexpr/symbolize.mc"
include "mexpr/pprint.mc"
include "mexpr/ast-builder.mc"

lang Infer

  syn InferMethod =

  syn Const =
  | CInfer { method: Option InferMethod }

end

lang CorePPL = MExprAst + MExprANF + MExprEq + MExprPrettyPrint + MExprSym 

  syn Const =
  | CWeight {}
  | CSample {}
  | CDist { dist: Dist }

  syn Dist =
  | DExp { a: Option Float }
  | DBern { p: Option Float }
  | DBeta { a: Option Float, b: Option Float }

  sem eqConst (lhs:Const) =
  | CWeight {} -> match lhs with CWeight _ then true else false
  | CSample {} -> match lhs with CSample _ then true else false
  | CDist { d = d2 } ->
    match lhs with CDist { d = d1 } then
      eqDist d1 d2
    else None ()

  sem eqDist (lhs:Dist) =
  | DExp { a = a2 } ->
    match lhs with DExp { a = a1 } then
      eqConst a1 a2
    else None ()

  | DBern { p = p2 } ->
    match lhs with DBern { p = p1 } then
      eqConst p1 p2
    else None ()

  | DBeta { a = a2, b = b2 } ->
    match lhs with DBeta { a = a1, b = b1 } then
      if eqConst a1 a2 then
        eqConst b1 b2
      else None ()
    else None ()

  sem getConstStringCode (indent : Int) =
  | CWeight _ -> "weight"
  | CSample _ -> "sample"
  | CDist _ -> "dist"
  | DExp d -> join ["exp", pprintNewline (pprintIncr indent), d.a]
  | DBern d -> join ["bern", pprintNewline (pprintIncr indent), d.p]
  | DBeta d -> join ["beta", pprintNewline (pprintIncr indent), d.a, " ", d.b]
end

lang CorePPLSMC = CorePPL

  syn Const =
  | CResample {}

end

lang Empirical

  syn Dist =
  | DEmpirical { sample: [(Float, Expr)] }

end

-- Convenience functions for manually constructing ASTs

let infer_ = use Infer in lam m. CInfer {method = m}

let weight_ = use CorePPL in CWeight {}

let sample_ = use CorePPL in CSample {}

let cdist_ = use CorePPL in lam d. CDist {d = d}

let dexp_ = use CorePPL in lam a. DExp {a = a}

let dbern_ = use CorePPL in lam p. DBern {p = p}

let dbeta_ = use CorePPL in lam a. lam b. DBeta {a = a, b = b}

let resample_ = use CorePPLSMC in CResample {}

let dempirical_ = use Empirical in lam s. DEmpirical {sample = s}
