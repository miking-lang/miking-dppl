include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "sys.mc"

include "../coreppl.mc"
include "../src-location.mc"

-- Inference methods
include "importance/compile.mc"
include "naive-mcmc/compile.mc"
include "trace-mcmc/compile.mc"


-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
lang MExprCompile = MExprPPL + Externals
end
let mexprCompile: Options -> Expr -> Expr =
  use MExprCompile in
  lam options. lam prog.

    -- Load runtime and compile function
    let compiler =
      match options.method with "mexpr-importance" then compilerImportance
      else match options.method with "mexpr-naive-mcmc" then compilerNaiveMCMC
      else match options.method with "mexpr-trace-mcmc" then compilerTraceMCMC
      else
        error (join [
          "Unknown CorePPL to MExpr inference option:", options.method
        ])
    in match compiler with (runtime, compile) in

    let parse = use BootParser in parseMCoreFile {{
      defaultBootParserParseMCoreFileArg
      with eliminateDeadCode = false }
      with allowFree = true }
    in

    -- Parse runtime
    let runtime = parse (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime]) in

    -- Get external definitions from runtime-AST (input to next step)
    -- Remove duplicate external definitions from model

    -- Symbolize model (ignore free variables)
    let prog = symbolize prog in
    -- Type check model
    let prog = typeCheck prog in
    let resTy = tyTm prog in
    -- Apply inference-specific transformation
    let prog = compile prog in
    -- Put model in top-level model function
    let prog = ulet_ "model" (lams_ [("state", tycon_ "State")] prog) in

    -- Final code to run inference and print something useful.
    let post = bindall_ [
      ulet_ "res" (appf1_ (var_ "run") (var_ "model")),
      ulet_ "nc" (app_ (var_ "normConstant") (tupleproj_ 0 (var_ "res"))),
      app_ (var_ "printLn") (app_ (var_ "float2string") (var_ "nc"))
    ] in

    -- Combine runtime, model, and generated post
    let prog = bindall_ [runtime,prog,post] in

    -- Symbolize the complete program (maybe not even needed?)
    -- let prog = symbolize prog in

    -- Type check the final program before returning! (maybe we shouldn't actually do this here, but in mi instead?)

    -- Return complete program
    prog
