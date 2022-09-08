
include "string.mc"
include "seq.mc"
include "sys.mc"

include "coreppl-to-rootppl/rootppl.mc"
include "coreppl-to-rootppl/compile.mc"
include "coreppl-to-mexpr/compile.mc"

include "mexpr/mexpr.mc"

include "dppl-arg.mc"

let performInference = lam options: Options. lam ast.

  if isPrefix eqChar "mexpr-" options.method then

    error "Full program inference is not supported for MExpr inference methods"

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
