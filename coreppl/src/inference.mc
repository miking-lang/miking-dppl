
include "string.mc"
include "seq.mc"
include "sys.mc"

include "coreppl-to-rootppl/rootppl.mc"
include "coreppl-to-rootppl/compile.mc"
include "coreppl-to-mexpr/compile.mc"

include "build.mc"
include "dppl-arg.mc"

lang InferenceLang = DPPLParser + LoadRuntime
end

let performInference = lam options: Options. lam ast.
  use InferenceLang in
  if isPrefix eqChar "mexpr-" options.method then

    let inferMethod = parseInferMethod options.method in

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

    -- Combine the runtimes with the main AST and eliminate duplicate
    -- definitions due to files having common dependencies.
    let mainAst = bindall_ [
      ulet_ "printFun" (app_ (var_ "printRes") tyPrintFun),
      app_ (var_ "printFun") (app_ (var_ "run") (nvar_ modelId))] in
    let mainAst = combineRuntimes options runtimes mainAst in

    -- Treat the input AST as the (only) input model.
    let modelAsts = mapFromSeq cmpInferMethod
      [(inferMethod, mapFromSeq nameCmp [(modelId, (ast, []))])] in

    -- Compile the ast with the chosen inference algorithm (handled in
    -- coreppl-to-mexpr/compile.mc)
    let ast = mexprCompile options runtimes mainAst modelAsts in

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
