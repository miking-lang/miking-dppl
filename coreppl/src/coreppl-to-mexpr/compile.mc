include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "mexpr/utils.mc"
include "sys.mc"
include "map.mc"

include "../coreppl.mc"
include "../extract.mc"
include "../inference-common/smc.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "../transformation.mc"
include "../src-location.mc"

include "dists.mc"
include "runtimes.mc"

lang DPPLKeywordReplace = DPPLParser
  sem _makeError : Info -> String -> Expr
  sem _makeError info =
  | keywordStr ->
    let msg = join ["Cannot use ", keywordStr, " outside of inferred model"] in
    let toStr = lam msg.
      map
        (lam ch. TmConst {val = CChar {val = ch},
                          ty = TyChar {info = info}, info = info})
        msg
    in
    TmApp {
      lhs = TmConst {val = CError (), ty = TyUnknown {info = info}, info = info},
      rhs = TmSeq {tms = toStr msg,
                   ty = TySeq {ty = TyChar {info = info}, info = info},
                   info = info},
      ty = TyUnknown {info = info}, info = info}

  sem replaceDpplKeywords : Expr -> Expr
  sem replaceDpplKeywords =
  | TmAssume t -> _makeError t.info "assume"
  | TmObserve t -> _makeError t.info "observe"
  | TmWeight t -> _makeError t.info "weight"
  | TmResample t -> _makeError t.info "resample"
  | t -> smap_Expr_Expr replaceDpplKeywords t
end

lang DPPLTransformDist = DPPLParser + TransformDist
  -- Used to transform away distributions in the main AST.
  sem transformDistributions : Expr -> Expr
  sem transformDistributions =
  | t ->
    let t = mapPre_Expr_Expr transformTmDist t in
    replaceTyDist t
end

-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile =
  MExprPPL + Resample + Externals + DPPLParser + DPPLExtract + LoadRuntime +
  Transformation + DPPLKeywordReplace + DPPLTransformDist + MExprSubstitute +
  MExprANFAll

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
      printLn (mexprPPLToString ast)
    else ());

    ast

  sem mexprCompile : Options -> Runtimes -> Expr -> Expr
  sem mexprCompile options runtimes =
  | corepplAst ->
    -- Symbolize and type-check the CorePPL AST.
    let corepplAst = symbolize corepplAst in
    let corepplAst = typeCheck corepplAst in

    -- Extract the infer expressions to separate ASTs, one per inference
    -- method. The result consists of the provided AST, updated such that
    -- each infer is replaced with a call to the 'run' function provided by
    -- the chosen runtime. It also consists of one AST per inference method
    -- used in the program.
    match extractInfer runtimes.entries corepplAst with (corepplAst, models) in

    -- Compile the model ASTs.
    let modelAsts = compileModels options runtimes models in

    -- Transform distributions in the CorePPL AST to use MExpr code.
    let corepplAst = transformDistributions corepplAst in

    -- Symbolize any free occurrences in the CorePPL AST and in any of the
    -- models using the symbolization environment of the runtime AST.
    let runtimeSymEnv = addTopNames symEnvEmpty runtimes.ast in
    let corepplAst = symbolizeExpr runtimeSymEnv corepplAst in

    -- Replace uses of DPPL keywords in the main AST, i.e. outside of models,
    -- with errors. This code is unreachable unless the inferred models are
    -- also used outside of infers, which is an error.
    -- TODO(larshum, 2022-10-07): Detect such errors statically.
    let corepplAst = replaceDpplKeywords corepplAst in

    -- Combine the CorePPL AST with the runtime AST, after extracting the
    -- models, and eliminate duplicate code due to common dependencies.
    let mainAst = bind_ runtimes.ast corepplAst in
    match eliminateDuplicateCodeWithSummary mainAst with (replaced, mainAst) in

    -- Apply the replacements performed by the duplicate code elimination on
    -- the model ASTs.
    let modelAsts = replaceIdentifiers replaced modelAsts in

    -- Insert all models into the main AST at the first point where any of the
    -- models are used.
    let prog = insertModels modelAsts mainAst in

    -- Type-check if options is set
    (if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog; ()
    else ());

    -- Return complete program
    prog

  sem compileModels : Options -> Runtimes -> Map Name ModelRepr -> Map Name Expr
  sem compileModels options runtimes =
  | models ->
    mapMapWithKey
      (lam id. lam model.
        match model with {ast = ast, method = method, params = params} in
        match loadCompiler options method with (_, compile) in
        match mapLookup method runtimes.entries with Some entry then
          let ast = transformModelAst options ast in
          let ast = compileModel compile entry id model in
          removeModelDefinitions ast
        else
          match pprintInferMethod 0 pprintEnvEmpty method with (_, methodStr) in
          error (join ["Runtime definition missing for (", methodStr, ")"]))
      models

  -- Removes all definitions of types, constructors, and externals from the
  -- model AST.
  --
  -- NOTE(larshum, 2022-10-22): We assume that the model code does not contain
  -- local definitions, but that any that are included are due to the
  -- extraction including all dependencies. Under this assumption, the
  -- definition is also present in the CorePPL program, and thus we can safely
  -- remove it from the model code.
  sem removeModelDefinitions : Expr -> Expr
  sem removeModelDefinitions =
  | TmType t -> removeModelDefinitions t.inexpr
  | TmConDef t -> removeModelDefinitions t.inexpr
  | TmExt t -> removeModelDefinitions t.inexpr
  | t -> smap_Expr_Expr removeModelDefinitions t

  sem _replaceHigherOrderConstant: Const -> Option Expr
  sem _replaceHigherOrderConstant =
  | CMap _ -> Some (var_ "map")
  | CMapi _ -> Some (var_ "mapi")
  | CIter _ -> Some (var_ "iter")
  | CIteri _ -> Some (var_ "iteri")
  | CFoldl _ -> Some (var_ "foldl")
  | CFoldr _ -> Some (var_ "foldr")
  | CCreate _ -> Some (var_ "create")
  | _ -> None ()

  sem _replaceHigherOrderConstantExpr: Expr -> Expr
  sem _replaceHigherOrderConstantExpr =
  | TmConst r ->
    match _replaceHigherOrderConstant r.val with Some t then
      withType r.ty (withInfo r.info t)
    else TmConst r
  | t -> t

  sem replaceHigherOrderConstants: Expr -> Expr
  sem replaceHigherOrderConstants =
  | t ->
    let t = mapPre_Expr_Expr _replaceHigherOrderConstantExpr t in
    let replacements =
      parseMCoreFile {
        defaultBootParserParseMCoreFileArg with
          eliminateDeadCode = false,
          allowFree = true
        } (join [corepplSrcLoc, "/coreppl-to-mexpr/runtime-const.mc"])
    in
    let replacements = normalizeTerm replacements in
    let t = bind_ replacements t in
    let t = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } t
    in
    t

  sem compileModel : ((Expr,Expr) -> Expr) -> RuntimeEntry -> Name -> ModelRepr -> Expr
  sem compileModel compile entry modelId =
  | {ast = modelAst, params = modelParams} ->

    -- ANF
    let modelAst = normalizeTerm modelAst in

    -- ANF with higher-order intrinsics replaced with alternatives in
    -- seq-native.mc
    -- TODO(dlunde,2022-10-24): @Lars I'm not sure how I should combine this
    -- with your updates.
    let modelAstNoHigherOrderConstants = replaceHigherOrderConstants modelAst in

    -- Apply inference-specific transformation
    let ast = compile (modelAst, modelAstNoHigherOrderConstants) in

    -- Bind the model code in a let-expression, which we can insert in the main
    -- AST.
    let stateVarId = nameNoSym "state" in
    let ast =
      nulet_ modelId
        (nlams_ (snoc modelParams (stateVarId, ntycon_ entry.stateId)) ast) in

    -- Replace any occurrences of TyDist in the program with the runtime
    -- distribution type. This needs to be performed after the previous step as
    -- captured parameters may have type TyDist.
    let ast = replaceTyDist ast in

    -- Symbolize the AST using the symbolization environment of the runtime
    -- corresponding to the inference method used for this model. This ensures
    -- that we refer to the functions defined in that particular runtime.
    symbolizeExpr entry.topSymEnv ast

  sem replaceIdentifiers : Map Name Name -> Map Name Expr -> Map Name Expr
  sem replaceIdentifiers replaced =
  | modelAsts ->
    mapMapWithKey
      (lam. lam modelAst. substituteIdentifiers replaced modelAst)
      modelAsts

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

-- TODO(dlunde,2022-10-19): We should also add a `simpleInfer` test that uses
-- the new infer keyword.
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

let truefn = lam. lam. true in

-- TODO(dlunde,2022-10-19): "with debugMExprCompile = true" currently has no
-- effect. This should be fixed so that we also parse and type check the
-- runtime files as part of the utests.
let dummyOptions = {default with debugMExprCompile = true} in

let compileModel = lam methodStr. lam modelAst.
  use MExprCompile in
  let inferMethod = inferMethodFromOptions dummyOptions methodStr in
  match loadCompiler dummyOptions inferMethod with (runtime, compile) in
  let entry = loadRuntimeEntry inferMethod runtime in
  let modelRepr = {ast = modelAst, method = inferMethod, params = []} in
  compileModel compile entry modelId modelRepr
in

-- Simple tests that ensure compilation of a simple model throws no errors
-- TODO(dlunde,2022-10-24): These tests are not good enough. We need to also
-- test individual options for the different methods (i.e., align and CPS)
utest compileModel "mexpr-importance" simple with () using truefn in
utest compileModel "mexpr-apf" simple with () using truefn in
utest compileModel "mexpr-bpf" simple with () using truefn in
utest compileModel "mexpr-mcmc-naive" simple with () using truefn in
utest compileModel "mexpr-mcmc-trace" simple with () using truefn in
utest compileModel "mexpr-mcmc-lightweight" simple with () using truefn in

()
