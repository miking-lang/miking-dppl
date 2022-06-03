include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "sys.mc"

include "../coreppl.mc"
include "../inference-common/smc.mc"
include "../src-location.mc"
include "../parser.mc"
include "../dppl-arg.mc"

-- Inference methods
include "importance/compile.mc"
include "importance-cps/compile.mc"
include "naive-mcmc/compile.mc"
include "trace-mcmc/compile.mc"


-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile = MExprPPL + Resample + Externals
end
let mexprCompile: Options -> Expr -> Expr =
  use MExprCompile in
  lam options. lam prog.

    -- Load runtime and compile function
    let compiler =
      match      options.method with "mexpr-importance" then compilerImportance
      else match options.method with "mexpr-importance-cps" then compilerImportanceCPS
      else match options.method with "mexpr-naive-mcmc" then compilerNaiveMCMC
      else match options.method with "mexpr-trace-mcmc" then compilerTraceMCMC
      else
        error (join [
          "Unknown CorePPL to MExpr inference method:", options.method
        ])

    in match compiler with (runtime, compile) in

    let parse = use BootParser in parseMCoreFile {{
      defaultBootParserParseMCoreFileArg
      with eliminateDeadCode = false }
      with allowFree = true }
    in

    -- Type check model
    let prog = typeCheck prog in
    let resTy = tyTm prog in

    -- Parse runtime
    let runtime = parse (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime]) in

    -- Get external definitions from runtime-AST (input to next step)
    let externals = getExternalIds runtime in

    -- Remove duplicate external definitions from model
    -- TODO Move this later, after the inference-specific compile (needed for CPS)
    let prog = removeExternalDefs externals prog in

    -- Symbolize model (ignore free variables)
    -- TODO Add symbolize option to also _not_ symbolize externals (we need to
    -- leave externals in the model program in order for CPS to work)
    let prog = symbolizeAllowFree prog in

    -- Apply inference-specific transformation
    let prog = compile prog in

    -- Construct record accessible in runtime (currently empty)
    let pre = ulet_ "compileOptions" (urecord_ [
    ]) in

    -- Put model in top-level model function
    let prog = ulet_ "model" (lams_ [("state", tycon_ "State")] prog) in

    -- Printing function for return type
    let tyPrintFun =
      match resTy with TyInt _ then   (var_ "int2string")
      else match resTy with TyFloat _ then (var_ "float2string")
      else match resTy with TyBool _ then (var_ "bool2string")
      else error "Return type cannot be printed"
    in

    let post = bindall_ [
      ulet_ "printFun" (app_ (var_ "printRes") tyPrintFun),
      appf2_ (var_ "run") (var_ "model") (var_ "printFun")
    ] in

    -- Combine runtime, model, and generated post
    let prog = bindall_ [pre,runtime,prog,post] in

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
utest mexprCompile {default with method = "mexpr-importance" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-importance-cps" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-naive-mcmc" } simple
with () using lam. lam. true in
utest mexprCompile {default with method = "mexpr-trace-mcmc" } simple
with () using lam. lam. true in

()
