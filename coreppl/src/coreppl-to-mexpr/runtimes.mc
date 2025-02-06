include "mexpr/symbolize.mc"
include "mexpr/utils.mc"

include "../parser.mc"

-- Inference methods
include "smc-apf/compile.mc"
include "smc-bpf/compile.mc"
include "is-lw/compile.mc"
include "mcmc-naive/compile.mc"
include "mcmc-trace/compile.mc"
include "mcmc-lightweight/compile.mc"
include "pmcmc-pimh/compile.mc"

lang LoadRuntime =
  MExprSym + MExprFindSym + MExprSubstitute +
  ImportanceSamplingMethod + BPFMethod + APFMethod +
  LightweightMCMCMethod  + NaiveMCMCMethod + TraceMCMCMethod +
  PIMHMethod

  type InferRuntimeEntry = {
    -- An AST representation of the runtime
    ast : Expr,

    -- The identifier of the run function for this runtime.
    runId : Name,

    -- The type for which State is an alias for this runtime.
    stateType : Type,

    -- A symbolization environment containing the identifiers defined in the
    -- top-level of the runtime program. This environment is used to symbolize
    -- the model AST so that it refers to definitions in its corresponding
    -- runtime.
    topSymEnv : SymEnv

  }

  type InferRuntimes = {
    -- Maps each kind of infer method (ignoring configuration parameters) to
    -- information about the runtime it uses.
    entries : Map InferMethod InferRuntimeEntry,

    -- A combined AST containing the code of all runtimes, after eliminating
    -- duplicates found in multiple of them.
    ast : Expr
  }

  sem loadCompiler : Options -> InferMethod
                       -> (String, InferenceInterface -> Expr)
  sem loadCompiler options =
  | Importance _ -> compilerImportance options
  | APF _ -> compilerAPF options
  | BPF _ -> compilerBPF options
  | LightweightMCMC _ -> compilerLightweightMCMC options
  | NaiveMCMC _ -> compilerNaiveMCMC options
  | TraceMCMC _ -> compilerTraceMCMC options
  | PIMH _ -> compilerPIMH options
  | _ -> error "Unsupported CorePPL to MExpr inference method"
end
