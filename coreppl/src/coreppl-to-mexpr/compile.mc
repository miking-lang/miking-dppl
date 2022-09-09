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

-- Inference methods
include "importance/compile.mc"
include "mcmc-naive/compile.mc"
include "mcmc-trace/compile.mc"
include "mcmc-aligned/compile.mc"
include "smc/compile.mc"


-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile = MExprPPL + Resample + Externals + DPPLExtract
  sem _addNameToRunBinding : Name -> Expr -> Expr
  sem _addNameToRunBinding runId =
  | TmLet t ->
    if and (not (nameHasSym t.ident)) (eqString (nameGetStr t.ident) "run") then
      TmLet {t with ident = runId}
    else TmLet {t with inexpr = _addNameToRunBinding runId t.inexpr}
  | t -> smap_Expr_Expr (_addNameToRunBinding runId) t

  -- TODO(larshum, 2022-09-09): Replace strings with constructor types?
  sem loadCompiler : Options -> String -> (String, Expr -> Expr)
  sem loadCompiler options =
  | "mexpr-importance" -> compilerImportance options
  | "mexpr-mcmc-naive" -> compilerNaiveMCMC options
  | "mexpr-mcmc-trace" -> compilerTraceMCMC options
  | "mexpr-mcmc-aligned" -> compilerAlignedMCMC options
  | "mexpr-smc" -> compilerSMC options
  | _ ->
    error (join ["Unknown CorePPL to MExpr inference method:", options.method])

  sem mexprCompile : Options -> Expr -> Map String [ModelData] -> Expr
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

    if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog
    else ();

    -- Return complete program
    prog

  sem compileInferenceMethod : Options -> [Expr] -> (String, [ModelData]) -> ([Expr], [Expr])
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

    -- Get external definitions from runtime-AST
    let externals = getExternalIds runtime in

    let modelAsts = map (compileModel compile externals) models in

    (snoc acc runtime, modelAsts)

  sem compileModel : (Expr -> Expr) -> Set String -> ModelData -> Expr
  sem compileModel compile externals =
  | model ->
    let desymbolizeExternals = lam prog.
      recursive let rec = lam env. lam prog.
        match prog with TmExt ({ ident = ident, inexpr = inexpr } & b) then
          let noSymIdent = nameNoSym (nameGetStr ident) in
          let env =
            if nameHasSym ident then (mapInsert ident noSymIdent env) else env
          in
          TmExt { b with ident = noSymIdent, inexpr = rec env inexpr }
        else match prog with TmVar ({ ident = ident } & b) then
          let ident =
            match mapLookup ident env with Some ident then ident else ident in
          TmVar { b with ident = ident }
        else smap_Expr_Expr (rec env) prog
      in rec (mapEmpty nameCmp) prog
    in

    -- Symbolize model (ignore free variables and externals)
    let prog = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } model.ast
    in

    -- Desymbolize externals in case any were symbolized beforehand
    let prog = desymbolizeExternals prog in

    -- Apply inference-specific transformation
    let prog = compile prog in

    -- Remove duplicate external definitions in model (already included in the
    -- runtime)
    let prog = removeExternalDefs externals prog in

    -- Put model in top-level model function
    nulet_ model.modelId
      (nlams_ (snoc model.params (nameSym "state", tycon_ "State")) prog)
end

let mexprCompile = use MExprCompile in mexprCompile

mexpr

let mexprCompile = lam method. lam e.
  use DPPLExtract in
  let inferData = defaultInferData method in
  mexprCompile {default with method = method} inferData e
in

let parse = parseMExprPPLString in

let simple = parse "
let x = assume (Beta 10.0 5.0) in
let obs = true in
observe obs (Bernoulli x);
x
" in

-- Simple tests that ensure compilation throws no errors
utest mexprCompile "mexpr-importance" simple
with () using lam. lam. true in
utest mexprCompile "mexpr-mcmc-naive" simple
with () using lam. lam. true in
utest mexprCompile "mexpr-mcmc-trace" simple
with () using lam. lam. true in
utest mexprCompile "mexpr-mcmc-aligned" simple
with () using lam. lam. true in
utest mexprCompile "mexpr-smc" simple
with () using lam. lam. true in

()
