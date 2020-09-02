include "mexpr/symbolize.mc"

lang PPLCore = PPLCore + MExprSym
  sem symbolize (env : Env) =
  | TmWeight { arg = expr } -> TmWeight { arg = symbolize env expr }
  | TmSampleExp { a = expr } -> TmSampleExp { a = symbolize env expr}
  | TmSampleBern { p = expr } -> TmSampleBern { p = symbolize env expr}
  | TmResample {} -> TmResample {}
end
