
include "string.mc"

include "../coreppl/rootppl/rootppl.mc"
include "../coreppl/rootppl/compile.mc"
include "../coreppl/mexpr/importance.mc"

include "dppl-arg.mc"

let performInference = lam options: Options. lam ast.
  match options.method with "importance" then
    importanceSamplingInference options ast
  else match options.method with "rootppl-smc" then
    -- dprint ast
    writeFile "out.cu" (printCompiledRPProg (rootPPLCompile options ast))
    -- print (join ["TODO: perform SMC using RootPPL with ",
    --              int2string options.particles, " particles."])
  else
    print "Please specify inference method using flag -m\n\n";
    print "Example:\n";
    print "  midppl example.mc -m rootppl-smc\n";
    exit 1
