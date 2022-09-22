include "dppl-arg.mc"
include "mexpr/mexpr.mc"

let buildMExpr = lam options. lam ast.
  -- Output the compiled mexpr code
  let outName = "out.mc" in
  writeFile outName (use MExpr in concat "mexpr\n" (mexprToString ast));

  -- Output the compiled OCaml code (unless --skip-final is specified)
  if options.skipFinal then ()
  else
    let res = sysRunCommand ["mi", "compile", outName] "" "." in
    if eqi res.returncode 0 then ()
    else
      error (join ["Compilation of generated MExpr code failed\nstdout:\n",
                   res.stdout, "\nstderr:\n", res.stderr])
