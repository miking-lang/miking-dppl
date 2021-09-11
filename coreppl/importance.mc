

include "ocaml/compile.mc"
include "seq.mc"




let importanceSamplingInference = lam options. lam ast.
    let options = {optimize = true, libraries = ["owl"]} in
    let code = "Printf.printf \"Hello %f\\n\" (Owl_stats.beta_rvs ~a:2. ~b:2.); " in
    let cunit = ocamlCompileWithConfig options code in
    let res = cunit.run "" [] in
    cunit.cleanup ();
    print (join ["Output: ", res.stdout, "\n"]);
    print "TODO importance\n"
