include "mexpr/ast-builder.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/externals.mc"
include "mexpr/type.mc"
include "mexpr/utils.mc"
include "sys.mc"

include "../coreppl.mc"
include "../extract.mc"
include "../inference-common/smc.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "../transformation.mc"

include "./common.mc"

-- Inference methods
include "apf/compile.mc"
include "bpf/compile.mc"
include "importance/compile.mc"
include "mcmc-naive/compile.mc"
include "mcmc-trace/compile.mc"
include "mcmc-lightweight/compile.mc"
include "smc/compile.mc"

lang LoadRuntime = DPPLParser + MExprFindSym + MExprEliminateDuplicateCode
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
      let methodStr = inferMethodToString method in
      let missingImpl =
        match get ids 0 with None _ then "run"
        else "State" in
      error (join ["Missing implementation of ", missingImpl, " in the ",
                   methodStr, " runtime"])

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

  sem combineRuntimes : Options -> Map InferMethod RuntimeEntry -> Expr
                     -> (Map InferMethod RuntimeEntry, Expr)
  sem combineRuntimes options runtimes =
  | ast ->
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

    -- Concatenate the runtime record and the runtime ASTs with the main AST.
    let ast = bindall_ (join [[pre], runtimeAsts, [ast]]) in

    -- Eliminate duplicate code in the combined AST, and update the
    -- symbolization environment of the runtime entries accordingly.
    match eliminateDuplicateCodeWithSummary ast with (replacements, ast) in
    let runtimes =
      mapMapWithKey
        (lam. lam runtime. updateSymEnv replacements runtime)
        runtimes in
    (runtimes, ast)
end

-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile =
  MExprPPL + Resample + Externals + DPPLParser + DPPLExtract + LoadRuntime +
  Transformation

  sem transformModelAst : Options -> Expr -> Expr
  sem transformModelAst options =
  | modelAst ->
    -- Transform the model AST, if the flag is set
    let ast =
      if options.transform then
        transform modelAst
      else modelAst in

    -- Optionally print the model AST
    (if options.printModel then
      use DPPLParser in printLn (mexprPPLToString ast)
    else ());

    ast

  sem mexprCompile : Options -> Map InferMethod RuntimeEntry -> Expr
                  -> Map Name ModelRepr -> Expr
  sem mexprCompile options runtimes mainAst =
  | modelAsts ->
    let modelAsts =
      mapMapWithKey
        (lam id. lam model.
          match model with {ast = ast, method = method, params = params} in
          match loadCompiler options method with (_, compile) in
          match mapLookup method runtimes with Some entry then
            let modelAst = transformModelAst options ast in
            compileModel compile entry id model
          else error (concat "Runtime definition not found for method "
                             (inferMethodToString method)))
        modelAsts in

    -- We use a map from model identifier to model AST, as we do not have to
    -- distinguish between which inference method they use at this point.
    let modelMap : Map Name Expr =
      mapFoldWithKey
        (lam acc. lam id. lam model.
          mapInsert id model acc)
        (mapEmpty nameCmp) modelAsts in

    -- Insert all models into the main AST at the first point where any of the
    -- models are used.
    let prog = insertModels modelMap mainAst in

    if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog
    else ();

    -- Return complete program
    prog

  sem compileModel : (Expr -> Expr) -> RuntimeEntry -> Name -> ModelRepr -> Expr
  sem compileModel compile entry modelId =
  | {ast = modelAst, params = modelParams} ->

    -- Symbolize model using the symbolization environment from the top-level
    -- of its corresponding runtime as a starting point.
    let prog = symbolizeExpr entry.topSymEnv modelAst in

    -- Apply inference-specific transformation
    let prog = compile prog in

    -- Update all unsymbolized references to refer to their corresponding
    -- definition in the runtime AST.
    match entry.topSymEnv with {varEnv = varEnv, conEnv = conEnv, tyConEnv = tyConEnv} in
    let stateVarId = nameSym "state" in
    let unsymIds =
      mapUnion
        (mapUnion (mapUnion varEnv conEnv) tyConEnv)
        (mapFromSeq cmpString (map (lam id. (nameGetStr id, id)) [stateVarId]))
    in
    let prog = replaceUnsymbolizedIds unsymIds prog in

    nulet_ modelId
      (nlams_ (snoc modelParams (stateVarId, ntycon_ entry.stateId)) prog)

  sem replaceUnsymbolized : Map String Name -> Name -> Name
  sem replaceUnsymbolized ids =
  | id ->
    if not (nameHasSym id) then
      optionGetOrElse (lam. id) (mapLookup (nameGetStr id) ids)
    else id

  sem replaceUnsymbolizedIds : Map String Name -> Expr -> Expr
  sem replaceUnsymbolizedIds ids =
  | TmVar t ->
    TmVar {t with ident = replaceUnsymbolized ids t.ident,
                  ty = replaceUnsymbolizedIdsType ids t.ty}
  | TmType t ->
    TmType {t with ident = replaceUnsymbolized ids t.ident,
                   tyIdent = replaceUnsymbolizedIdsType ids t.tyIdent,
                   inexpr = replaceUnsymbolizedIds ids t.inexpr,
                   ty = replaceUnsymbolizedIdsType ids t.ty}
  | TmConDef t ->
    TmConDef {t with ident = replaceUnsymbolized ids t.ident,
                     tyIdent = replaceUnsymbolizedIdsType ids t.tyIdent,
                     inexpr = replaceUnsymbolizedIds ids t.inexpr,
                     ty = replaceUnsymbolizedIdsType ids t.ty}
  | TmConApp t ->
    TmConApp {t with ident = replaceUnsymbolized ids t.ident,
                     body = replaceUnsymbolizedIds ids t.body,
                     ty = replaceUnsymbolizedIdsType ids t.ty}
  | t ->
    let t = smap_Expr_Expr (replaceUnsymbolizedIds ids) t in
    let t = smap_Expr_Type (replaceUnsymbolizedIdsType ids) t in
    let t = smap_Expr_Pat (replaceUnsymbolizedIdsPat ids) t in
    withType (replaceUnsymbolizedIdsType ids (tyTm t)) t

  sem replaceUnsymbolizedIdsType : Map String Name -> Type -> Type
  sem replaceUnsymbolizedIdsType ids =
  | TyCon t ->
    TyCon {t with ident = replaceUnsymbolized ids t.ident}
  | ty -> smap_Type_Type (replaceUnsymbolizedIdsType ids) ty

  sem replaceUnsymbolizedIdsPat : Map String Name -> Pat -> Pat
  sem replaceUnsymbolizedIdsPat ids =
  | PatCon t ->
    PatCon {t with ident = replaceUnsymbolized ids t.ident,
                   subpat = replaceUnsymbolizedIdsPat ids t.subpat}
  | p ->
    let p = smap_Pat_Pat (replaceUnsymbolizedIdsPat ids) p in
    withTypePat (replaceUnsymbolizedIdsType ids (tyPat p)) p

  -- We insert all models before the first binding where any of them are used.
  -- This is simple but correct, as we only need them to be placed after the
  -- runtime code.
  sem insertModels : Map Name Expr -> Expr -> Expr
  sem insertModels models =
  | TmLet t ->
    if modelUsedInBody models false t.body then
      bindall_ (snoc (mapValues models) (TmLet t))
    else TmLet {t with inexpr = insertModels models t.inexpr}
  | TmRecLets t ->
    let modelUsedInBinding = lam bind. modelUsedInBody models false bind.body in
    if any identity (map modelUsedInBinding t.bindings) then
      bindall_ (snoc (mapValues models) (TmRecLets t))
    else TmRecLets {t with inexpr = insertModels models t.inexpr}
  | TmType t -> TmType {t with inexpr = insertModels models t.inexpr}
  | TmConDef t -> TmConDef {t with inexpr = insertModels models t.inexpr}
  | TmUtest t -> TmUtest {t with next = insertModels models t.next}
  | TmExt t -> TmExt {t with inexpr = insertModels models t.inexpr}
  | t -> bindall_ (snoc (mapValues models) t)

  sem modelUsedInBody : Map Name Expr -> Bool -> Expr -> Bool
  sem modelUsedInBody models acc =
  | TmVar t ->
    if acc then acc
    else if mapMem t.ident models then true
    else false
  | t -> sfold_Expr_Expr (modelUsedInBody models) acc t
end

let mexprCompile = use MExprCompile in mexprCompile

mexpr

let parse = parseMExprPPLString in

let simple = parse "
let t : () -> Float = lam.
  let x = assume (Beta 10.0 5.0) in
  let obs = true in
  observe obs (Bernoulli x);
  x
in
t {}
" in
match use MExprFindSym in findNamesOfStrings ["t"] simple with [Some modelId] in

let compileModel = lam method. lam modelAst.
  use MExprCompile in
  let method = parseInferMethod method in
  match loadCompiler default method with (runtime, compile) in
  let entry = loadRuntimeEntry method runtime in
  compileModel compile entry modelId (modelAst, [])
in

-- Simple tests that ensure compilation throws no errors
utest compileModel "mexpr-importance" simple
with () using lam. lam. true in
-- NOTE(larshum, 2022-10-05): Not implemented yet
--utest compileModel "mexpr-mcmc-naive" simple
--with () using lam. lam. true in
--utest compileModel "mexpr-mcmc-trace" simple
--with () using lam. lam. true in
--utest compileModel "mexpr-mcmc-aligned" simple
--with () using lam. lam. true in
--utest compileModel "mexpr-mcmc-lightweight" simple
--with () using lam. lam. true in
--utest compileModel "mexpr-smc" simple
--with () using lam. lam. true in

()
