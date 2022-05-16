include "../dists.mc"
include "mexpr/ast-builder.mc"

lang MExprPPLImportance = MExprPPL + TransformDist

  -- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr
  -- AST types here. Optimally, the type would be Options -> CorePPLExpr ->
  -- MExprExpr or similar.
  sem compile : Expr -> Expr
  sem compile =
  | t ->
    -- Transform distributions to MExpr distributions
    let t = map_Expr_Expr transformTmDist t in

    -- Transform samples, observes, and weights to MExpr
    let t = map_Expr_Expr transformProb t in

    -- Transform observes to MExpr observes

    -- Transform weights to MExpr weights

    t

  sem transformProb =
  | TmAssume t -> withInfo t.info (app_ (recordproj_ "sample" t.dist) unit_)

  -- NOTE(dlunde,2022-05-16): Note that we cannot stop immediately when the
  -- weight becomes 0 (-inf in log-space). For this, we need CPS, PCFGs, or
  -- maybe some type of exception handler.
  | TmObserve t ->
    let weight = withInfo t.info (app_ (recordproj_ "logObserve" t.dist) t.value) in
    withInfo t.info (appf2_ (var_ "updateWeight") weight (var_ "state"))
  | TmWeight t ->
    withInfo t.info (app_ (var_ "updateWeight") t.weight)
  | t -> t

end

let compilerImportance = use MExprPPLImportance in
  ("importance/runtime.mc", compile)
