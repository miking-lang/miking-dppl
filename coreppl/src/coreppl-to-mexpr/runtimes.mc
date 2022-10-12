include "mexpr/symbolize.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/utils.mc"

include "../parser.mc"

-- Inference methods
include "apf/compile.mc"
include "bpf/compile.mc"
include "importance/compile.mc"
include "mcmc-naive/compile.mc"
include "mcmc-trace/compile.mc"
include "mcmc-lightweight/compile.mc"
include "smc/compile.mc"

lang LoadRuntime =
  DPPLParser + MExprSym + MExprFindSym + MExprEliminateDuplicateCode
  
  type RuntimeEntry = {
    -- An AST representation of the runtime
    ast : Expr,

    -- The identifier of the run function for this runtime.
    runId : Name,

    -- The identifier of the State type for this runtime.
    stateId : Name,

    -- A symbolization environment containing the identifiers defined in the
    -- top-level of the runtime program. This environment is used to symbolize
    -- the model AST so that it refers to definitions in its corresponding
    -- runtime.
    topSymEnv : SymEnv
  }

  sem loadCompiler : Options -> InferMethod -> (String, Expr -> Expr)
  sem loadCompiler options =
  | Importance _ -> compilerImportance options
  | BPF _ -> compilerBPF options
  | _ -> error "Unsupported CorePPL to MExpr inference method"

  sem loadRuntimes : Options -> Expr -> Map InferMethod RuntimeEntry
  sem loadRuntimes options =
  | ast -> loadRuntimesH options (mapEmpty cmpInferMethod) ast

  sem loadRuntimesH : Options -> Map InferMethod RuntimeEntry -> Expr
                   -> Map InferMethod RuntimeEntry
  sem loadRuntimesH options acc =
  | TmInfer t ->
    if mapMem t.method acc then acc
    else
      -- A runtime for the current infer method has not been loaded. Parse and
      -- symbolize the runtime of the corresponding method, and add it to the
      -- accumulated map.
      match loadCompiler options t.method with (runtime, _) in
      mapInsert t.method (loadRuntimeEntry t.method runtime) acc
  | t -> sfold_Expr_Expr (loadRuntimesH options) acc t

  sem loadRuntimeEntry : InferMethod -> String -> RuntimeEntry
  sem loadRuntimeEntry method =
  | runtime ->
    let parse = use BootParser in parseMCoreFile {
      defaultBootParserParseMCoreFileArg with
        eliminateDeadCode = false,
        allowFree = true
      }
    in
    let runtime = parse (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime]) in
    let runtime = symbolizeAllowFree runtime in
    match findRequiredRuntimeIds method runtime with (runId, stateId) in
    { ast = runtime, runId = runId, stateId = stateId
    , topSymEnv = addTopNames symEnvEmpty runtime }

  -- Finds a pre-defined list of identifiers in the given runtime AST, which
  -- are assumed to be present in all runtimes.
  sem findRequiredRuntimeIds : InferMethod -> Expr -> (Name, Name)
  sem findRequiredRuntimeIds method =
  | runtime ->
    let ids = findNamesOfStrings ["run", "State"] runtime in
    match ids with [Some runId, Some stateId] then (runId, stateId)
    else
      match pprintInferMethod 0 pprintEnvEmpty method with (_, methodStr) in
      let missingImpl =
        match get ids 0 with None _ then "run"
        else "State" in
      error (join ["Missing identifier ", missingImpl, " in the runtime of (",
                   methodStr, ")"])

  sem updateSymEnv : Map Name Name -> RuntimeEntry -> RuntimeEntry
  sem updateSymEnv replacements =
  | entry ->
    let replaceId = lam id.
      optionGetOrElse (lam. id) (mapLookup id replacements)
    in
    let replaceInEnv = lam env. mapMapWithKey (lam. lam id. replaceId id) env in
    let replaceInSymEnv = lam symEnv.
      {symEnv with varEnv = replaceInEnv symEnv.varEnv,
                   conEnv = replaceInEnv symEnv.conEnv,
                   tyConEnv = replaceInEnv symEnv.tyConEnv}
    in
    {entry with topSymEnv = replaceInSymEnv entry.topSymEnv}

  sem combineRuntimes : Options -> Map InferMethod RuntimeEntry
                     -> (Map InferMethod RuntimeEntry, Expr, SymEnv)
  sem combineRuntimes options =
  | runtimes ->
    -- Construct record accessible at runtime
    -- NOTE(dlunde,2022-06-28): It would be nice if we automatically lift the
    -- options variable here to an Expr.
    let pre = ulet_ "compileOptions" (urecord_ [
      ("resample", str_ options.resample),
      ("cps", str_ options.cps),
      ("printSamples", bool_ options.printSamples),
      ("earlyStop", bool_ options.earlyStop),
      ("mcmcLightweightGlobalProb", float_ options.mcmcLightweightGlobalProb),
      ("mcmcLightweightGlobalModProb", float_ options.mcmcLightweightGlobalModProb),
      ("printAcceptanceRate", bool_ options.printAcceptanceRate)
    ]) in

    let runtimeAsts = map (lam entry. entry.ast) (mapValues runtimes) in

    -- Concatenate the runtime record and the runtime ASTs.
    let ast = bindall_ (join [[pre], runtimeAsts]) in

    -- Eliminate duplicate code in the combined AST, and update the
    -- symbolization environment of the runtime entries accordingly.
    match eliminateDuplicateCodeWithSummary ast with (replacements, ast) in
    let runtimes =
      mapMapWithKey
        (lam. lam runtime. updateSymEnv replacements runtime)
        runtimes in
    (runtimes, ast, addTopNames symEnvEmpty ast)
end
