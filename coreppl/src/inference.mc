
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

    -- Compile the ast with the chosen inference algorithm (handled in
    -- coreppl-to-mexpr/compile.mc)
    let ast = mexprCompile options ast in

    -- Output the compiled mexpr code
    let outName = "out.mc" in
    writeFile outName (use MExpr in concat "mexpr\n" (mexprToString ast));

    -- Output the compiled OCaml code (unless --skip-final is specified)
    if options.skipFinal then ()
    else sysRunCommand ["mi", "compile", outName] "" "."; ()

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
