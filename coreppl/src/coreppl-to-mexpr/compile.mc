include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type.mc"
include "sys.mc"

include "../coreppl.mc"
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


-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile = MExprPPL + Resample + Externals
end
let mexprCompile: Options -> Expr -> Expr =
  use MExprCompile in
  lam options. lam prog.

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
        case "mexpr-mcmc-lightweight" then compilerLightweightMCMC options
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

    -- Type check model. NOTE(dlunde,2022-06-09): We do not want the
    -- annotations added by the type checker, as this may make the printed
    -- program unparsable. That's why we simply discard the result here (but,
    -- we first extract the return type).
    let tcprog = typeCheck prog in
    let resTy = tyTm tcprog in

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

    -- Get external definitions from runtime-AST (input to next step)
    let externals = getExternalIds runtime in

    -- Remove duplicate external definitions in model (already included in the
    -- runtime)
    let prog = removeExternalDefs externals prog in

    -- Put model in top-level model function
    let prog = ulet_ "model" (lams_ [("state", tycon_ "State")] prog) in

    -- Construct record accessible in runtime
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

    -- Printing function for return type
    let tyPrintFun =
      match resTy with TyInt _ then   (var_ "int2string")
      else match resTy with TyFloat _ then uconst_ (CFloat2string ())
      else match resTy with TyBool _ then (var_ "bool2string")
      else match resTy with TySeq { ty = TyChar _ } then (ulam_ "x" (var_ "x"))
      else error "Return type cannot be printed"
    in

    let post = bindall_ [
      ulet_ "printFun" (app_ (var_ "printRes") tyPrintFun),
      appf2_ (var_ "run") (var_ "model") (var_ "printFun")
    ] in

    -- Combine runtime, model, and generated post
    let prog = bindall_ [pre,runtime,prog,post] in

    if options.debugMExprCompile then
      -- Check that the combined program type checks
      typeCheck prog
    else ();

    -- Return complete program
    prog

mexpr

let parse = parseMExprPPLString in

let simple = parse "
let x = assume (Beta 10.0 5.0) in
let obs = true in
observe obs (Bernoulli x);
x
" in

-- Simple tests that ensure compilation throws no errors
utest mexprCompile {default with method = "mexpr-importance", cps = "none" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-importance", cps = "partial" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-importance", cps = "full" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-mcmc-naive" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-mcmc-trace" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-mcmc-lightweight" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-mcmc-lightweight", align = true } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-smc" } simple
with () using lam. lam. true in

()
