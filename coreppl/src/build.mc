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

  -- Compile to MExpr
  let outName = sysTempFileMake () in
  writeFile outName (use MExpr in concat "mexpr\n" (mexprToString ast));

  -- Make intermediate mc file visible in the current dir if option is set
  (if options.outputMc then
    sysCopyFile outName (concat options.output ".mc"); ()
  else ());

  -- Output the compiled OCaml code (unless --skip-final is specified)
  (if options.skipFinal then ()
   else
     let msg = "Compilation of generated MExpr code failed" in
     runCommandWithError
       ["mi", "compile",
        "--output", options.output,
        outName] msg
  );
  sysDeleteFile outName;
  ()

let buildRootPPL = lam options. lam ast.
  -- NOTE(dlunde,2023-06-28): It is not currently possible to specify the name
  -- of the compiled binary using rppl, but at least the intermediate CUDA file
  -- uses the correct base name given to cppl using --output.
  let outName = concat options.output ".cu" in
  writeFile outName (printCompiledRPProg ast);
  if options.skipFinal then ()
  else
    let msg = "RootPPL compilation failed" in
    let cmds = join [
      ["rppl", outName],
      ["--stack-size", int2string options.stackSize],
      match options.seed with Some seed then
        ["--seed", int2string seed]
      else []
    ] in
    runCommandWithError cmds msg
