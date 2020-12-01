include "ast.mc"
include "mexpr/anf.mc"

lang CorePPL = CorePPL + MExprANF

  sem isValue =
  | TmWeight _ -> false
  | TmSampleExp _ -> false
  | TmSampleBern _ -> false
  | TmResample _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmWeight { arg = arg } ->
    normalizeName (lam arg. k (TmWeight { arg = arg })) arg

  | TmSampleExp { a = a } ->
    normalizeName (lam a. k (TmSampleExp { a = a })) a

  | TmSampleBern { p = p } ->
    normalizeName (lam p. k (TmSampleBern { p = p })) p

  | TmResample {} -> k (TmResample {})

end

