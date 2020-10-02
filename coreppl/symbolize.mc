include "ast.mc"
include "mexpr/symbolize.mc"

lang PPLCore = PPLCore + MExprSym
  sem symbolizeExpr (env : Env) =
  | TmWeight { arg = expr } -> TmWeight { arg = symbolize env expr }
  | TmSampleExp { a = expr } -> TmSampleExp { a = symbolize env expr}
  | TmSampleBern { p = expr } -> TmSampleBern { p = symbolize env expr}
  | TmResample {} -> TmResample {}
end
