include "ocaml/mcore.mc"

let compileRunMCore = lam ast.
  let compileOcaml = lam libs. lam clibs. lam ocamlProg.
    let options = {optimize = true, libraries = libs, cLibraries = clibs} in
    let cunit = ocamlCompileWithConfig options ocamlProg in
    let res = cunit.run "" [] in
    cunit.cleanup ();
    print (join [res.stdout, "\n"]);
    ""
  in
  compileMCore ast {emptyHooks with compileOcaml = compileOcaml}
