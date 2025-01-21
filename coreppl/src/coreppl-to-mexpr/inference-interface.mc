include "mexpr/symbolize.mc"
include "mlang/loader.mc"
include "mexpr/lamlift.mc"

include "../dppl-arg.mc"

lang InferenceInterface = Sym + SymGetters
  type InferenceSymEnv =
    { env : {path : String, env : SymEnv}
    , lamliftSols : Map Name FinalOrderedLamLiftSolution
    }

  type InferenceInterface =
    { options : Options
    , runtime : InferenceSymEnv
    , dists : InferenceSymEnv
    , extractNormal : () -> Expr
    , extractNoHigherOrderConsts : () -> Expr
    , stateName : Name
    }

  sem appFromEnv
    : InferenceSymEnv
    -> String
    -> [Expr]
    -> Expr
  sem appFromEnv env fname = | args ->
    let n = _getVarExn fname env.env in
    match mapLookup n env.lamliftSols with Some sol then
      let extraArgs = map (lam pair. withType pair.1 (nvar_ pair.0)) sol.vars in
      appSeq_ (nvar_ n) (concat extraArgs args)
    else
      appSeq_ (nvar_ n) args
end
