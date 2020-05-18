include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "mexpr/ast-builder.mc"

lang PPLCoreAst = MExprAst + MExprPrettyPrint

  syn Expr =
  | TmWeight { arg: Expr }
  | TmSampleExp { a: Expr }
  | TmSampleBern { p: Expr }
  | TmResample {}

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

let weight_ = use PPLCoreAst in
  lam arg. TmWeight {arg = arg}

let sampleExp_ = use PPLCoreAst in
  lam a. TmSampleExp {a = a}

let sampleBern_ = use PPLCoreAst in
  lam p. TmSampleBern {p = p}

let resample_ = use PPLCoreAst in
  TmResample {}
