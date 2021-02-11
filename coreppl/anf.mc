include "ast.mc"
include "mexpr/anf.mc"

lang CorePPL = CorePPL + MExprANF

  sem isValue =
  | TmWeight _ -> false
  | TmSampleExp _ -> false
  | TmSampleBern _ -> false
  | TmResample _ -> false

  sem normalize (k : Expr -> Expr) =
  | TmWeight ({ arg = arg } & t) ->
    normalizeName (lam arg. k (TmWeight { t with arg = arg })) arg

  | TmSampleExp ({ a = a } & t) ->
    normalizeName (lam a. k (TmSampleExp { t with a = a })) a

  | TmSampleBern ({ p = p } & t) ->
    normalizeName (lam p. k (TmSampleBern { t with p = p })) p

  | TmResample t -> k (TmResample t)

end

