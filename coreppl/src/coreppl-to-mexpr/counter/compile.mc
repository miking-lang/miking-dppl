include "mexpr/ast-builder.mc"

lang MExprPPLCounter = MExprPPL
  sem compile : Options -> (Expr, Expr) -> Expr
  sem compile options =
  | (t, _) ->
    let t = mapPre_Expr_Expr countAssume t in
    t

  sem countAssume =
  | TmAssume t ->
    let i = withInfo t.info in
    i (app_ (i (var_ "incrementCounter")) (i (var_ "counter")));
    TmAssume t
  | t -> t

end

let compilerCounter = lam options.
  use MExprPPLCounter in
  ("counter/runtime.mc", compile options)
