include "dppl-arg.mc"
include "mexpr/mexpr.mc"
include "sys.mc"

include "coreppl-to-rootppl/compile.mc"

let runCommandWithError = lam cmds. lam errMsg.
  let res = sysRunCommand cmds "" "." in
  if eqi res.returncode 0 then ()
  else
    error (join [errMsg, "\nstdout:\n", res.stdout, "\nstderr:\n", res.stderr])

let buildMExpr = lam options. lam ast.
  -- Output the compiled mexpr code
  let outName = "out.mc" in
  writeFile outName (use MExpr in concat "mexpr\n" (mexprToString ast));

  -- Output the compiled OCaml code (unless --skip-final is specified)
  if options.skipFinal then ()
  else
    let msg = "Compilation of generated MExpr code failed" in
    runCommandWithError ["mi", "compile", outName] msg

let buildRootPPL = lam options. lam ast.
  let outName = "out.cu" in
  writeFile outName (printCompiledRPProg ast);
  if options.skipFinal then ()
  else
    let msg = "Compilation of generated CUDA code failed" in
    let cmds = ["rppl", outName, "--stack-size" , int2string options.stackSize] in
    runCommandWithError cmds msg
