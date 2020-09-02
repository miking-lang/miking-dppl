-- Pretty printing for PPLCore

include "ast.mc"
include "mexpr/pprint.mc"

lang PPLCore = PPLCore + MExprPrettyPrint

  sem pprintCode (indent : Int) =
  | TmWeight t ->
    let arg = pprintCode indent t.arg in
    join ["weight(", arg, ")"]
  | TmSampleExp t ->
    let a = pprintCode indent t.a in
    join ["sampleExp(", a, ")"]
  | TmSampleBern t ->
    let p = pprintCode indent t.p in
    join ["sampleBern(", p, ")"]
  | TmResample _ -> "resample"

end
