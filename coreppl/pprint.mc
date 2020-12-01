-- Pretty printing for CorePPL

include "ast.mc"
include "mexpr/pprint.mc"

lang CorePPL = CorePPL + MExprPrettyPrint

  sem pprintCode (indent : Int) (env: Env) =
  | TmWeight t ->
    match printParen (pprintIncr indent) env t.arg with (env,arg) then
      (env, join ["weight", pprintNewline (pprintIncr indent), arg])
    else never
  | TmSampleExp t ->
    match printParen (pprintIncr indent) env t.a with (env,a) then
      (env,join ["sampleExp", pprintNewline (pprintIncr indent), a])
    else never
  | TmSampleBern t ->
    match printParen (pprintIncr indent) env t.p with (env,p) then
      (env,join ["sampleBern", pprintNewline (pprintIncr indent), p])
    else never
  | TmResample _ -> (env,"resample")

end
