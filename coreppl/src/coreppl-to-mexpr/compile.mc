include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "mexpr/utils.mc"
include "sys.mc"
include "map.mc"

include "../coreppl.mc"
include "../inference/smc.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "../transformation.mc"
include "../src-location.mc"

include "extract.mc"
include "backcompat.mc"
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
  MExprANFAll + CPPLBackcompat

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

  sem mexprCpplCompile : Options -> Bool -> Expr -> Expr
  sem mexprCpplCompile options noInfer =
  | ast ->

    -- First translate all Default {} inference methods
    let ast = replaceDefaultInferMethod options ast in

    -- Load the runtimes used in the provided AST, and collect identifiers of
    -- common methods within the runtimes.
    let runtimes = loadRuntimes options ast in

    -- If no infers are found, the entire AST is the model code, so we transform
    -- it as:
    --
    -- let d = infer <method> (lam. <model>) in
    -- let printRes = ... in
    -- printRes <pp> d
    --
    -- where <method> = inference method chosen according to options
    --       <model> = the entire AST
    --       <pp> = the pretty-print function used to print the result
    match
      if noInfer then programModelTransform options ast
      else (runtimes, ast)
    with (runtimes, ast) in

    -- Combine the required runtime ASTs to one AST and eliminate duplicate
    -- definitions due to files having common dependencies. The result is an
    -- updated map of runtime entries, a combined runtime AST and a
    -- symbolization environment.
    let runtimes = combineRuntimes options runtimes in

    mexprCompile options runtimes ast


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
    match extractInfer options runtimes.entries corepplAst with (corepplAst, models) in

    -- Compile the model ASTs.
    let modelAsts = compileModels options runtimes models in

    -- Transform distributions in the CorePPL AST to use MExpr code.
    let corepplAst = transformDistributions corepplAst in
    --printLn "--------------------";printLn (expr2str corepplAst);
    printLn "--------------------";printLn (options.output);

    -- Symbolize any free occurrences in the CorePPL AST and in any of the
    -- models using the symbolization environment of the runtime AST.
    --printLn "========================";printLn (expr2str runtimes.ast);
    let runtimeSymEnv = addTopNames symEnvEmpty runtimes.ast in
    let corepplAst = symbolizeExpr runtimeSymEnv corepplAst in
    --printLn "--------------------";printLn (expr2str corepplAst);

    -- Replace uses of DPPL keywords in the main AST, i.e. outside of models,
    -- with errors. This code is unreachable unless the inferred models are
    -- also used outside of infers, which is an error.
    -- TODO(larshum, 2022-10-07): Detect such errors statically.
    let corepplAst = replaceDpplKeywords corepplAst in
    --printLn "--------------------";printLn (expr2str corepplAst);

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

    -- TODO(dlunde,2023-05-22): Does not work, currently (the program does not
    -- type check at this stage). It does, however, type check after generating
    -- the code and compiling it with Miking.
    -- Type-check if options is set
    -- (if options.debugMExprCompile then
    --   -- Check that the combined program type checks
    --   typeCheck prog; ()
    -- else ());

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
          let ast = compileModel compile entry id {model with ast = ast} in
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
        (nlams_ (snoc modelParams (stateVarId, entry.stateType)) ast) in

    -- Replace any occurrences of TyDist in the program with the runtime
    -- distribution type. This needs to be performed after the previous step as
    -- captured parameters may have type TyDist.
    let ast = replaceTyDist ast in

    let #var"" =
      printLn "printing environment:";
      mapFoldWithKey (lam. lam k: String. lam v: Name.
        printLn (join ["    ", k, "<", nameGetStr v, " | ", int2string (sym2hash (optionGetOr _noSymbol (nameGetSym v))), ">"])
      ) () entry.topSymEnv.tyConEnv
    in

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
  | TmUtest t ->
    if or (modelUsedInBody models false t.test)
          (modelUsedInBody models false t.expected) then
      bindall_ (snoc (mapValues models) (TmUtest t))
    else
      TmUtest {t with next = insertModels models t.next}
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

lang TestCompileLang =
  MExprCompile + CPPLBackcompat + MExprFindSym + DPPLParser
end

mexpr

use TestCompileLang in

let parse = parseMExprPPLString in

-- TODO(dlunde,2022-10-19): We should also add a `simpleInfer` test that uses
-- the new infer keyword.
let simple = parse "
let x = assume (Beta 10.0 5.0) in
let obs = true in
observe obs (Bernoulli x);
x
" in
let simple = symbolize simple in

let truefn = lam. lam. true in

let dummyOptions = default in

let compile = lam options. lam methodStr. lam ast.
  let options = {options with method = methodStr} in
  match programModelTransform options ast with (runtimes, ast) in
  let runtimeData = combineRuntimes options runtimes in
  mexprCompile options runtimeData ast
in

-- Simple tests that ensure compilation of a simple model throws no errors

-- Likelihood weighting
utest compile {dummyOptions with cps = "none"}
        "is-lw" simple with () using truefn in
utest compile {dummyOptions with cps = "partial"}
        "is-lw" simple with () using truefn in
utest compile {dummyOptions with cps = "full"}
        "is-lw" simple with () using truefn in

-- APF
utest compile dummyOptions "smc-apf" simple with () using truefn in

-- BPF
utest compile dummyOptions "smc-bpf" simple with () using truefn in

-- Naive MCMC
utest compile dummyOptions "mcmc-naive" simple with () using truefn in

-- Trace MCMC
utest compile dummyOptions "mcmc-trace" simple with () using truefn in

-- Lightweight MCMC
utest compile dummyOptions "mcmc-lightweight" simple with () using truefn in
utest compile {dummyOptions with align = true, cps = "none"}
        "mcmc-lightweight" simple with () using truefn in
utest compile {dummyOptions with align = true, cps = "partial"}
        "mcmc-lightweight" simple with () using truefn in
utest compile {dummyOptions with align = true, cps = "full"}
        "mcmc-lightweight" simple with () using truefn in

utest compile dummyOptions "pmcmc-pimh" simple with () using truefn in

()
