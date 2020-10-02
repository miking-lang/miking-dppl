-- Pretty printing for PPLCore

include "ast.mc"
include "mexpr/pprint.mc"

lang PPLCore = PPLCore + MExprPrettyPrint

  sem pprintCode (indent : Int) (env: Env) =
  | TmWeight t ->
    match printParen (incr indent) env t.arg with (env,arg) then
      (env, join ["weight", newline (incr indent), arg])
    else never
  | TmSampleExp t ->
    match printParen (incr indent) env t.a with (env,a) then
      (env,join ["sampleExp", newline (incr indent), a])
    else never
  | TmSampleBern t ->
    match printParen (incr indent) env t.p with (env,p) then
      (env,join ["sampleBern", newline (incr indent), p])
    else never
  | TmResample _ -> (env,"resample")

end
