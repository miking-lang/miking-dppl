include "mexpr/duplicate-code-elimination.mc"
include "mexpr/symbolize.mc"
include "mexpr/utils.mc"

include "../parser.mc"
include "../src-location.mc"

-- Inference methods
include "smc-apf/compile.mc"
include "smc-bpf/compile.mc"
include "is-lw/compile.mc"
include "mcmc-naive/compile.mc"
include "mcmc-trace/compile.mc"
include "mcmc-lightweight/compile.mc"
include "pmcmc-pimh/compile.mc"

lang LoadRuntime =
  DPPLParser +
  MExprSym + MExprFindSym + MExprSubstitute + MExprEliminateDuplicateCode

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
                       -> (String, (Expr,Expr) -> Expr)
  sem loadCompiler options =
  | Importance _ -> compilerImportance options
  | APF _ -> compilerAPF options
  | BPF _ -> compilerBPF options
  | LightweightMCMC _ -> compilerLightweightMCMC options
  | NaiveMCMC _ -> compilerNaiveMCMC options
  | TraceMCMC _ -> compilerTraceMCMC options
  | PIMH _ -> compilerPIMH options
  | _ -> error "Unsupported CorePPL to MExpr inference method"

  sem loadRuntimes : Options -> Expr -> Map InferMethod InferRuntimeEntry
  sem loadRuntimes options =
  | ast -> loadRuntimesH options (mapEmpty cmpInferMethod) ast

  sem loadRuntimesH : Options -> Map InferMethod InferRuntimeEntry -> Expr
                   -> Map InferMethod InferRuntimeEntry
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

  sem loadRuntime : Bool -> String -> Expr
  sem loadRuntime eliminateDeadCode =
  | runtime ->
    let parse = use BootParser in parseMCoreFile {
      defaultBootParserParseMCoreFileArg with
        eliminateDeadCode = eliminateDeadCode,
        allowFree = true
      }
    in
    parse (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime])

  -- `makeRuntimeNameMap runtime ids` creates a map from string representations
  -- of identifiers in the runtime `runtime` to their corresponding name in the
  -- runtime. The domain of this map is the seqence of identifiers `ids`.
  sem makeRuntimeNameMap : Expr -> [String] -> (String -> Name)
  sem makeRuntimeNameMap runtime =| ids ->
    let names = findNamesOfStrings ids runtime in
    let names =
      zipWith
        (lam id. lam name.
          optionGetOrElse
            (lam. error (join [
              "Did not find \"", id, "\" in the runtime.\n",
              "Make sure it is not removed by dead code elimination."
            ]))
            name)
        ids names
    in
    let idNameMap = mapFromSeq cmpString (zip ids names) in
    lam str.
      mapFindOrElse
        (lam. error (join [
          "\"", str, "\" is not in the domain of this runtime name map"
        ]))
        str idNameMap

  sem loadRuntimeEntry : InferMethod -> String -> InferRuntimeEntry
  sem loadRuntimeEntry method =
  | runtime ->
    let runtime = symbolizeAllowFree (loadRuntime false runtime) in
    match findRequiredRuntimeIds method runtime with (runId, stateId) in
    let stateType = findStateType method stateId runtime in
    let pruningRuntime = loadRuntime false "pruning/runtime.mc" in
    let delayedRuntime = loadRuntime false "delayed-sampling/runtime.mc" in
    let runtime = bindall_ [pruningRuntime, delayedRuntime, runtime] in
    match eliminateDuplicateCodeWithSummary runtime with (replacements, runtime) in
    { ast = runtime, runId = runId, stateType = stateType
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

  sem findStateType : InferMethod -> Name -> Expr -> Type
  sem findStateType method stateId =
  | runtime ->
    match findStateTypeH stateId (None ()) runtime with Some ty then ty
    else
      match pprintInferMethod 0 pprintEnvEmpty method with (_, methodStr) in
      error (join [ "Could not find State type definition in runtime of ("
                  , methodStr, ")" ])

  sem findStateTypeH : Name -> Option Type -> Expr -> Option Type
  sem findStateTypeH stateId acc =
  | TmType {ident = id, params = params, tyIdent = tyIdent, inexpr = inexpr} ->
    if nameEq id stateId then
      -- Create a type application for each type parameter of the State type.
      recursive let g = lam ty. lam params.
        match params with [_] ++ params then
          tyapp_ ty (TyUnknown {info = infoTy ty})
        else ty
      in
      let display = g (ntycon_ stateId) params in

      -- Replace all inner type variables with TyUnknown to let the
      -- type-checker can correctly infer them.
      recursive let unknownTyVars = lam ty.
        match ty with TyVar t then TyUnknown {info = t.info}
        else smap_Type_Type unknownTyVars ty
      in
      let content = unknownTyVars tyIdent in
      Some (TyAlias {display = display, content = content})
    else findStateTypeH stateId acc inexpr
  | t -> sfold_Expr_Expr (findStateTypeH stateId) acc t

  -- Combines a sequence `runtimes` of runtime terms which are combined from
  -- left to right, discarding the final "inexpr" term. Duplicated code is
  -- removed from the combined runtime and all references are updated in the
  -- term `tm`. This function assumes that all terms are symbolized.
  sem combineRuntimes : [Expr] -> Expr -> (Expr, Expr)
  sem combineRuntimes runtimes =| tm ->
    match eliminateDuplicateCodeWithSummary (bindall_ runtimes)
      with (replacements, runtime)
    in
    (runtime, substituteIdentifiers replacements tm)

  -- Combines non-inference runtime with an inference runtime (from left to
  -- right). Duplicated code is removed from the combined runtime and all
  -- references are updated in the term `tm`.
  sem combineRuntimeWithInferRuntime
    : Expr -> InferRuntime -> Expr -> (InferRuntime, Expr)
  sem combineRuntimeWithInferRuntime runtime inferRuntime =| tm ->
    match
      eliminateDuplicateCodeWithSummary (bindall_ [runtime, inferRuntime.ast])
      with (replacements, runtime)
    in
    ({
      entries = _updateRuntimeEntriesSymEnv replacements inferRuntime.entries,
      ast = runtime
    },
     substituteIdentifiers replacements tm)

  -- Combines inference runtimes, removing duplicate code.
  sem combineInferRuntimes
    : Options -> Map InferMethod InferRuntimeEntry -> InferRuntimes
  sem combineInferRuntimes options =
  | entries ->
    -- Construct record accessible at runtime
    -- NOTE(dlunde,2022-06-28): It would be nice if we automatically lift the
    -- options variable here to an Expr.
    let pre = ulet_ "compileOptions" (urecord_ [
      ("resample", str_ options.resample),
      ("cps", str_ options.cps),
      ("printSamples", bool_ options.printSamples),
      ("earlyStop", bool_ options.earlyStop),
      ("mcmcLightweightGlobalProb", float_ options.mcmcLightweightGlobalProb),
      ("mcmcLightweightReuseLocal", bool_ options.mcmcLightweightReuseLocal),
      ("printAcceptanceRate", bool_ options.printAcceptanceRate),
      ("subsample", bool_ options.subsample),
      ("subsampleSize", int_ options.subsampleSize),

      -- NOTE(dlunde,2022-11-04): Emulating option type
      ("seedIsSome",
       match options.seed with Some seed then bool_ true else bool_ false),
      ("seed", match options.seed with Some seed then int_ seed else int_ 0)

    ]) in

    let runtimeAsts = map (lam entry. entry.ast) (mapValues entries) in

    -- Concatenate the runtime record and the runtime ASTs.
    let ast = bindall_ (join [[pre], runtimeAsts]) in

    -- Eliminate duplicate code in the combined AST of all runtimes, and update
    -- the symbolization environments of the individual runtimes accordingly.
    match eliminateDuplicateCodeWithSummary ast with (replacements, ast) in
    {entries = _updateRuntimeEntriesSymEnv replacements entries, ast = ast}

  sem _updateRuntimeEntriesSymEnv
    : Map Name Name -> Map InferMethod InferRuntimeEntry
      -> Map InferMethod InferRuntimeEntry
  sem _updateRuntimeEntriesSymEnv replacements =| entries ->
    mapMapWithKey
      (lam. lam entry.
        {entry with topSymEnv = _updateSymEnv replacements entry.topSymEnv})
      entries

  sem _updateSymEnv : Map Name Name -> SymEnv -> SymEnv
  sem _updateSymEnv replacements =
  | symEnv ->
    let replaceId = lam id.
      optionGetOrElse (lam. id) (mapLookup id replacements)
    in
    let replaceInEnv = lam env. mapMapWithKey (lam. lam id. replaceId id) env in
    let replaceInSymEnv = lam symEnv.
      let symEnv = symbolizeUpdateVarEnv symEnv (replaceInEnv symEnv.currentEnv.varEnv) in
      let symEnv = symbolizeUpdateConEnv symEnv (replaceInEnv symEnv.currentEnv.conEnv) in
      let symEnv = symbolizeUpdateTyConEnv symEnv (replaceInEnv symEnv.currentEnv.tyConEnv) in
      symEnv
    in
    replaceInSymEnv symEnv
end
