include "mexpr/ast-builder.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "mexpr/utils.mc"
include "sys.mc"

include "../coreppl.mc"
include "../extract.mc"
include "../inference-common/smc.mc"
include "../src-location.mc"
include "../parser.mc"
include "../dppl-arg.mc"

-- Inference methods
include "importance/compile.mc"
include "mcmc-naive/compile.mc"
include "mcmc-trace/compile.mc"
include "mcmc-lightweight/compile.mc"
include "smc/compile.mc"

lang LoadRuntime = DPPLParser + MExprFindSym + MExprEliminateDuplicateCode
  type RuntimeEntry = {
    ast : Expr,
    runId : Name,
    updateWeightId : Name,
    stateId : Name
  }

  sem loadCompiler : Options -> InferMethod -> (String, Expr -> Expr)
  sem loadCompiler options =
  | Importance _ -> compilerImportance options
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
    let runtime = symbolizeExpr {symEnvEmpty with allowFree = true} runtime in
    match collectRuntimeIds method runtime
    with (runId, updateWeightId, stateId) in
    { ast = runtime, runId = runId, updateWeightId = updateWeightId
    , stateId = stateId }

  sem collectRuntimeIds : InferMethod -> Expr -> (Name, Name, Name)
  sem collectRuntimeIds method =
  | runtime ->
    let ids = findNamesOfStrings ["run", "updateWeight", "State"] runtime in
    match ids with [Some runId, Some updateWeightId, Some stateId] then
      (runId, updateWeightId, stateId)
    else
      let methodStr = inferMethodToString method in
      let missingImpl =
        match get ids 0 with None () then "run"
        else match get ids 1 with None () then "updateWeight"
        else "State" in
      error (join ["Missing implementation of ", missingImpl, " ",
                   "in the ", methodStr, " runtime"])

  sem combineRuntimes : Options -> Map InferMethod RuntimeEntry -> Expr -> Expr
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

    -- Concatenate the runtime record and the runtime ASTs with the main AST,
    -- and eliminate any duplicate code due to common dependencies.
    let ast = bindall_ (join [[pre], runtimeAsts, [ast]]) in
    eliminateDuplicateCode ast
end

-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile =
  MExprPPL + Resample + Externals + DPPLParser + DPPLExtract + LoadRuntime

  sem _addNameToRunBinding : Name -> Expr -> Expr
  sem _addNameToRunBinding runId =
  | TmLet t ->
    if and (not (nameHasSym t.ident)) (eqString (nameGetStr t.ident) "run") then
      TmLet {t with ident = runId}
    else TmLet {t with inexpr = _addNameToRunBinding runId t.inexpr}
  | t -> smap_Expr_Expr (_addNameToRunBinding runId) t

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
                  -> Map InferMethod (Map Name ModelRepr) -> Expr
  sem mexprCompile options runtimes mainAst =
  | modelAsts ->
    let modelAsts =
      mapMapWithKey
        (lam method. lam models.
          match loadCompiler options method with (_, compile) in

          match mapLookup method runtimes with Some entry then
            mapMapWithKey
              (lam id. lam model.
                match model with (modelAst, modelParams) in
                let modelAst = transformModelAst options modelAst in
                compileModel compile entry id (modelAst, modelParams))
              models
          else error (concat "Runtime definition not found for method "
                             (inferMethodToString method)))
        modelAsts in

    -- We use a map from model identifier to model AST, as we do not have to
    -- distinguish between which inference method they use at this point.
    let modelMap : Map Name Expr =
      mapFoldWithKey
        (lam acc. lam method. lam models.
          mapFoldWithKey
            (lam acc. lam id. lam model.
              mapInsert id model acc)
            acc models)
        (mapEmpty nameCmp) modelAsts in

    -- Insert all models into the main AST at the first point where any of the
    -- models are used.
    let prog = insertModels modelMap mainAst in

    -- Add the correct symbols to the references to runtime functions by the
    -- model AST code.
    let prog = symbolizeRuntimeReferences prog in

    if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog
    else ();

    -- Return complete program
    prog

  sem compileModel : (Expr -> Expr) -> RuntimeEntry -> Name -> ModelRepr -> Expr
  sem compileModel compile entry modelId =
  | (modelAst, modelParams) ->
    -- Symbolize model (ignore free variables and externals)
    let prog = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } modelAst
    in

    -- Apply inference-specific transformation
    let prog = compile prog in

    let prog = replaceUnsymbolizedId entry.updateWeightId prog in
    let prog = replaceUnsymbolizedId entry.stateId prog in

    nulet_ modelId
      (nlams_ (snoc modelParams (nameSym "state", ntycon_ entry.stateId)) prog)

    sem replaceUnsymbolizedId : Name -> Expr -> Expr
    sem replaceUnsymbolizedId id =
    | TmVar t ->
      if and (not (nameHasSym t.ident)) (nameEqStr t.ident id) then
        TmVar {t with ident = id,
                      ty = replaceUnsymbolizedIdType id t.ty}
      else TmVar {t with ty = replaceUnsymbolizedIdType id t.ty}
    | t ->
      let t = withType (replaceUnsymbolizedIdType id (tyTm t)) t in
      let t = smap_Expr_Type (replaceUnsymbolizedIdType id) t in
      smap_Expr_Expr (replaceUnsymbolizedId id) t

  sem replaceUnsymbolizedIdType : Name -> Type -> Type
  sem replaceUnsymbolizedIdType id =
  | TyCon t ->
    if and (not (nameHasSym t.ident)) (nameEqStr t.ident id) then
      TyCon {t with ident = id}
    else TyCon t
  | ty -> smap_Type_Type (replaceUnsymbolizedIdType id) ty

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

  sem symbolizeRuntimeReferences : Expr -> Expr
  sem symbolizeRuntimeReferences =
  | t -> symbolizeRuntimeReferencesExpr (mapEmpty nameCmp) t

  sem symbolizeRuntimeReferencesExpr : Map Name Name -> Expr -> Expr
  sem symbolizeRuntimeReferencesExpr names =
  | TmVar t ->
    match mapLookup t.ident names with Some id then
      TmVar {t with ident = id}
    else TmVar t
  | TmConApp t ->
    match mapLookup t.ident names with Some id then
      TmConApp {t with ident = id}
    else TmConApp t
  | TmLet t ->
    let names = _addName t.ident names in
    TmLet {t with body = symbolizeRuntimeReferencesExpr names t.body,
                  inexpr = symbolizeRuntimeReferencesExpr names t.inexpr}
  | TmRecLets t ->
    let symbolizeBinding = lam names. lam bind.
      {bind with body = symbolizeRuntimeReferencesExpr names bind.body}
    in
    let names =
      foldl
        (lam names. lam bind. _addName bind.ident names)
        names t.bindings in
    TmRecLets {t with bindings = map (symbolizeBinding names) t.bindings,
                      inexpr = symbolizeRuntimeReferencesExpr names t.inexpr}
  | TmType t ->
    let names = _addName t.ident names in
    TmType {t with inexpr = symbolizeRuntimeReferencesExpr names t.inexpr}
  | TmConDef t ->
    let names = _addName t.ident names in
    TmConDef {t with inexpr = symbolizeRuntimeReferencesExpr names t.inexpr}
  | TmExt t ->
    let names = _addName t.ident names in
    TmExt {t with inexpr = symbolizeRuntimeReferencesExpr names t.inexpr}
  | t -> smap_Expr_Expr (symbolizeRuntimeReferencesExpr names) t

  sem _addName : Name -> Map Name Name -> Map Name Name
  sem _addName id =
  | names -> mapInsert (nameNoSym (nameGetStr id)) id names
end

let mexprCompile = use MExprCompile in mexprCompile

mexpr

let compileModel = lam method. lam modelAst.
  use MExprCompile in
  match loadCompiler default method with (_, compile) in
  let model = {defaultModelData method with ast = modelAst} in
  compileModel compile model
in

let parse = parseMExprPPLString in

let simple = parse "
let x = assume (Beta 10.0 5.0) in
let obs = true in
observe obs (Bernoulli x);
x
" in

-- Simple tests that ensure compilation throws no errors
utest compileModel "mexpr-importance" simple
with () using lam. lam. true in
utest compileModel "mexpr-mcmc-naive" simple
with () using lam. lam. true in
utest compileModel "mexpr-mcmc-trace" simple
with () using lam. lam. true in
utest compileModel "mexpr-mcmc-aligned" simple
with () using lam. lam. true in
utest compileModel "mexpr-mcmc-lightweight" simple
with () using lam. lam. true in
utest compileModel "mexpr-smc" simple
with () using lam. lam. true in

()
