
include "string.mc"
include "seq.mc"
include "sys.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/externals.mc"

include "coreppl-to-rootppl/rootppl.mc"
include "coreppl-to-rootppl/compile.mc"
include "coreppl-to-mexpr/compile.mc"

include "build.mc"
include "dppl-arg.mc"

lang InferenceLang =
  DPPLParser + LoadRuntime + MExprEliminateDuplicateCode + Externals
end

let performInference = lam options: Options. lam ast.
  use InferenceLang in
  if isPrefix eqChar "mexpr-" options.method then

    let inferMethod = inferMethodFromOptions options options.method in

    match loadCompiler options inferMethod with (runtime, _) in

    let runtimes = mapFromSeq cmpInferMethod
      [(inferMethod, loadRuntimeEntry inferMethod runtime)] in

    let modelId = nameSym "model" in

    -- Printing function for return type
    let resTy = tyTm (typeCheck ast) in
    let tyPrintFun =
      match resTy with TyInt _ then   (var_ "int2string")
      else match resTy with TyFloat _ then uconst_ (CFloat2string ())
      else match resTy with TyBool _ then (var_ "bool2string")
      else match resTy with TySeq { ty = TyChar _ } then (ulam_ "x" (var_ "x"))
      else error "Return type cannot be printed"
    in

    -- Combine the runtime ASTs to one AST, and eliminate duplicate definitions
    -- due to runtimes having common dependencies.
    match combineRuntimes options runtimes with (runtimes, runtimeAst, symEnv) in

    -- Produce the main AST, which in this case just prints the result, and
    -- combine it with the runtime AST using its symbolization environment.
    let inferConfig = inferMethodConfig (NoInfo ()) inferMethod in
    let mainAst = bindall_ [
      ulet_ "printFun" (app_ (var_ "printRes") tyPrintFun),
      app_ (var_ "printFun")
        (appf2_ (var_ "run") inferConfig (nvar_ modelId))] in
    let mainAst = symbolizeExpr symEnv mainAst in
    let mainAst = bind_ runtimeAst mainAst in

    -- Treat the input AST as the (only) input model.
    let modelRepr = {ast = ast, method = inferMethod, params = []} in
    let models = mapFromSeq nameCmp [(modelId, modelRepr)] in

    -- Compile the ast with the chosen inference algorithm (handled in
    -- coreppl-to-mexpr/compile.mc)
    let ast = mexprCompile options runtimes mainAst models in

    buildMExpr options ast

  else match options.method with "rootppl-smc" then
    let outName = "out.cu" in

    let ast = rootPPLCompile options ast in
    writeFile outName (printCompiledRPProg ast);
    if options.skipFinal then ()
    else
      sysRunCommand [
        "rootppl", outName,
        "--stack-size", (int2string options.stackSize)
      ] "" "."; ()

	else
    print "Please specify inference method using flag -m\n\n";
    print "Example:\n";
    print "  cppl example.mc -m rootppl-smc\n";
    exit 1
