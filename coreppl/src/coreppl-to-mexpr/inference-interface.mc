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
    { options : TransformationOptions
    , runtime : InferenceSymEnv
    , dists : InferenceSymEnv
    , extraEnvs : Map String InferenceSymEnv
    -- NOTE(vipa, 2025-04-09): These take a function to run on the
    -- *entire* AST before extracting the model-relevant code
    , extractNormal : (Expr -> Expr) -> Expr
    , extractNoHigherOrderConsts : (Expr -> Expr) -> Expr
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
