include "ast.mc"
include "mexpr/symbolize.mc"

lang CorePPL = CorePPL + MExprSym
  sem symbolizeExpr (env : Env) =
  | TmWeight { arg = expr } -> TmWeight { arg = symbolizeExpr env expr }
  | TmSampleExp { a = expr } -> TmSampleExp { a = symbolizeExpr env expr}
  | TmSampleBern { p = expr } -> TmSampleBern { p = symbolizeExpr env expr}
  | TmResample {} -> TmResample {}
end
