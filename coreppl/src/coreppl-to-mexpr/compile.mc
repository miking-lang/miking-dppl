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

  sem mexprCompile : Options -> InferData -> Expr -> Expr
  sem mexprCompile options inferData =
  | prog ->
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

    -- Load runtime and compile function
    let compiler: (String, Expr -> Expr) =
      switch options.method
        case "mexpr-importance" then compilerImportance options
        case "mexpr-mcmc-naive" then compilerNaiveMCMC options
        case "mexpr-mcmc-trace" then compilerTraceMCMC options
        case "mexpr-mcmc-aligned" then compilerAlignedMCMC options
        case "mexpr-smc" then compilerSMC options
        case _ then error (
          join [ "Unknown CorePPL to MExpr inference method:", options.method ]
        )
      end
    in

    match compiler with (runtime, compile) in

    let parse = use BootParser in parseMCoreFile {
      defaultBootParserParseMCoreFileArg with
        eliminateDeadCode = false,
        allowFree = true
      }
    in

    -- Symbolize model (ignore free variables and externals)
    let prog = symbolizeExpr
      { symEnvEmpty with allowFree = true, ignoreExternals = true } prog
    in

    -- Desymbolize externals in case any were symbolized beforehand
    let prog = desymbolizeExternals prog in

    -- Apply inference-specific transformation
    let prog = compile prog in

    -- Parse runtime
    let runtime = parse (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime]) in

    -- Assign a pre-defined name to the 'run' binding.
    -- TODO(larshum, 2022-09-08): Is it safe to assume this binding always
    -- exists, and that it is unique in the runtime program?
    let runtime = _addNameToRunBinding inferData.runId runtime in

    -- Get external definitions from runtime-AST (input to next step)
    let externals = getExternalIds runtime in

    -- Remove duplicate external definitions in model (already included in the
    -- runtime)
    let prog = removeExternalDefs externals prog in

    -- Put model in top-level model function
    let prog =
      nulet_ inferData.modelId
        (nlams_ (snoc inferData.params (nameSym "state", tycon_ "State")) prog) in

    -- Construct record accessible in runtime
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

    -- Combine runtime and model
    let prog = bindall_ [pre,runtime,prog] in

    if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog
    else ();

    -- Return complete program
    prog
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
