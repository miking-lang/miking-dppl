include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "mexpr/ast-builder.mc"

lang PPLCoreAst = MExprAst + MExprPrettyPrint

  syn Expr =
  | TmWeight { arg: Expr }
  | TmSample { arg: Expr }
  | TmResample {}

  sem pprintCode (indent : Int) =
  | TmWeight t ->
    let arg = pprintCode indent t.arg in
    strJoin "" ["weight(", arg, ")"]
  | TmSample t ->
    let arg = pprintCode indent t.arg in
    strJoin "" ["sample(", arg, ")"]
  | TmResample _ -> "resample"

end

let weight_ = use PPLCoreAst in
  lam arg. TmWeight {arg = arg}

let sample_ = use PPLCoreAst in
  lam arg. TmSample {arg = arg}

let resample_ = use PPLCoreAst in
  TmResample {}
