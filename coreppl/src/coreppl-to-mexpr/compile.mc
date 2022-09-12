include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "sys.mc"

include "../coreppl.mc"
include "../extract.mc"
include "../inference-common/smc.mc"
include "../src-location.mc"
include "../parser.mc"
include "../dppl-arg.mc"
include "dedup.mc"

-- Inference methods
include "importance/compile.mc"
include "mcmc-naive/compile.mc"
include "mcmc-trace/compile.mc"
include "mcmc-aligned/compile.mc"
include "smc/compile.mc"


-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile = MExprPPL + Resample + Externals + DPPLParser + DPPLExtract + CPPLDedup
  sem _addNameToRunBinding : Name -> Expr -> Expr
  sem _addNameToRunBinding runId =
  | TmLet t ->
    if and (not (nameHasSym t.ident)) (eqString (nameGetStr t.ident) "run") then
      TmLet {t with ident = runId}
    else TmLet {t with inexpr = _addNameToRunBinding runId t.inexpr}
  | t -> smap_Expr_Expr (_addNameToRunBinding runId) t

  sem loadCompiler : Options -> InferMethod -> (String, Expr -> Expr)
  sem loadCompiler options =
  | ImportanceSampling _ -> compilerImportance options
  | _ -> error "Unsupported CorePPL to MExpr inference method"

  sem mexprCompile : Options -> Expr -> Map InferMethod [ModelData] -> Expr
  sem mexprCompile options mainAst =
  | methodInferData ->
    -- Construct record accessible at runtime
    -- NOTE(dlunde,2022-06-28): It would be nice if we automatically lift the
    -- options variable here to an Expr.
    let pre = ulet_ "compileOptions" (urecord_ [
      ("resample", str_ options.resample),
      ("cps", str_ options.cps),
      ("printSamples", bool_ options.printSamples),
      ("earlyStop", bool_ options.earlyStop),
      ("mcmcAlignedGlobalProb", float_ options.mcmcAlignedGlobalProb),
      ("mcmcAlignedGlobalModProb", float_ options.mcmcAlignedGlobalModProb)
    ]) in

    match mapAccumL (compileInferenceMethod options) [] (mapBindings methodInferData)
    with (runtimes, modelAsts) in

    let prog = bindall_ (join [[pre], runtimes, join modelAsts, [mainAst]]) in

    -- Remove duplicate definitions of types and declarations of externals. To
    -- do this, we first symbolize the program, then remove the symbols of
    -- types and external declarations prior to removing duplicates. The result
    -- is that references to different "versions" of external declarations end
    -- up referring to the same after re-symbolization of the AST.
    let prog = symbolize prog in
    let prog = deduplicateTypesAndExternals prog in
    let prog = symbolize prog in

    if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog
    else ();

    -- Return complete program
    prog

  sem compileInferenceMethod : Options -> [Expr] -> (InferMethod, [ModelData])
                            -> ([Expr], [Expr])
  sem compileInferenceMethod options acc =
  | (inferMethod, models) ->
    match loadCompiler options inferMethod with (runtime, compile) in

    let parse = use BootParser in parseMCoreFile {
      defaultBootParserParseMCoreFileArg with
        eliminateDeadCode = false,
        allowFree = true
      }
    in

    -- Parse runtime
    let runtime = parse (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime]) in

    -- Assign a pre-defined name to the 'run' binding.
    -- TODO(larshum, 2022-09-08): Is it safe to assume this binding always
    -- exists, and that it is unique in every runtime program?
    let runId =
      let fst : ModelData = head models in
      fst.runId
    in
    let runtime = _addNameToRunBinding runId runtime in

    let modelAsts = map (compileModel compile) models in

    (snoc acc runtime, modelAsts)

  sem compileModel : (Expr -> Expr) -> ModelData -> Expr
  sem compileModel compile =
  | model ->
    -- Symbolize model (ignore free variables and externals)
    let prog = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } model.ast
    in

    -- Apply inference-specific transformation
    let prog = compile prog in

    -- Put model in top-level model function
    nulet_ model.modelId
      (nlams_ (snoc model.params (nameSym "state", tycon_ "State")) prog)
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
utest compileModel "mexpr-smc" simple
with () using lam. lam. true in

()
