-- Pretty printing for PPLCore

include "ast.mc"
include "mexpr/pprint.mc"

lang PPLCorePrettyPrint = PPLCoreAst + MExprPrettyPrint

  sem pprintCode (indent : Int) =
  | TmWeight t ->
    let arg = pprintCode indent t.arg in
    strJoin "" ["weight(", arg, ")"]
  | TmSampleExp t ->
    let a = pprintCode indent t.a in
    strJoin "" ["sampleExp(", a, ")"]
  | TmSampleBern t ->
    let p = pprintCode indent t.p in
    strJoin "" ["sampleBern(", p, ")"]
  | TmResample _ -> "resample"

end
