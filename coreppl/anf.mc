include "ast.mc"
include "mexpr/anf.mc"

lang PPLCore = PPLCore + MExprANF

  sem isValue =
  | TmWeight _ -> false
  | TmSampleExp _ -> false
  | TmSampleBern _ -> false
  | TmResample _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmWeight { arg = arg } ->
    normalizeName (lam arg. bind k (TmWeight { arg = arg })) arg

  | TmSampleExp { a = a } ->
    normalizeName (lam a. bind k (TmSampleExp { a = a })) a

  | TmSampleBern { p = p } ->
    normalizeName (lam p. bind k (TmSampleBern { p = p })) p

  | TmResample {} -> k (TmResample {})

end

