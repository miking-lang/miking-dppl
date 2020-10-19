include "ast.mc"
include "mexpr/eq.mc"

lang CorePPL = CorePPL + MExprEq

  sem eqExprH (env : Env) (free : Env) (lhs : Expr) =
  | TmWeight { arg = arg2 } ->
    match lhs with TmWeight { arg = arg1 } then
      eqExprH env free arg1 arg2
    else None ()

  | TmSampleExp { a = a2 } ->
    match lhs with TmSampleExp { a = a1 } then
      eqExprH env free a1 a2
    else None ()

  | TmSampleBern { p = p2 } ->
    match lhs with TmSampleBern { p = p1 } then
      eqExprH env free p1 p2
    else None ()

  | TmResample {} ->
    match lhs with TmResample {} then Some free else None ()

end
