
include "string.mc"

let performInference = lam options. lam ast.
  match options.method with "importance" then
    print "TODO: perform importance sampling!"
  else match options.method with "rootppl-smc" then
    print (join ["TODO: perform SMC using RootPPL with ",
                 int2string options.particles, " particles."])
  else
    print "Please specify inference method using flag -m\n\n";
    print "Example:\n";
    print "  midppl example.mc -m rootppl-smc\n";
    exit 1
