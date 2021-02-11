include "ast.mc"
include "mexpr/symbolize.mc"

lang CorePPL = CorePPL + MExprSym
  sem symbolizeExpr (env : Env) =
  | TmWeight ({ arg = expr } & t) -> TmWeight { t with arg = symbolizeExpr env expr }
  | TmSampleExp ({ a = expr } & t)-> TmSampleExp { t with a = symbolizeExpr env expr }
  | TmSampleBern ({ p = expr } & t) -> TmSampleBern { t with p = symbolizeExpr env expr }
  | TmResample t -> TmResample t
end
