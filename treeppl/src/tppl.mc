/-
TreePPL Compiler command line
-/

include "sys.mc"

include "mexpr/ast.mc"
include "mexpr/boot-parser.mc"
include "mexpr/type-check.mc"

include "treeppl-to-coreppl/compile.mc"
   
mexpr

let backend = "mexpr" in

if eqString backend "rootppl" then
  match argv with ![_, _, _] then -- TODO use argparse
    printLn "-- Error: arguments";
    printLn "-- Correct: tpplc program.tppl data.mc";
    exit 0
  else match argv with [_, filename, data] in
  use BootParser in
  let input = parseMCoreFile {defaultBootParserParseMCoreFileArg with   eliminateDeadCode = false}
    data in
  let content = readFile filename in
  use TreePPLAst in
  match parseTreePPLExn filename content with  file in
  
  use TreePPLCompile in
    let corePplAst: Expr = compile input file in
    --  TODO(vsenderov,2022-05-10): Maybe parse from the command line  
    let outfile = "out.cu" in
    let options = {default with method = "rootppl-smc"} in
    printLn "NEVER!";
    let prog: Expr = typeCheck corePplAst in
    --let prog = corePplAst in
    writeFile outfile (printCompiledRPProg (rootPPLCompile options prog));
    -- mexprCompile backend 
    print (join ["RootPPL output written.\n",
     "To get an executable, compile with \n\n  rootppl --stack-size 10000 ",   outfile, "\n\n"]);
    use MExprPPL in 
    print (concat (mexprPPLToString corePplAst) "\n\n")
    
else -- defaulting to MExpr
  match argv with ![_, _, _] then -- TODO use argparse
    printLn "-- Error: arguments";
    printLn "-- Correct: tpplc program.tppl data.mc";
    exit 0
  else match argv with [_, filename, data] in
  use BootParser in
  let input = parseMCoreFile {defaultBootParserParseMCoreFileArg with eliminateDeadCode = false}
    data in
  let content = readFile filename in
  use TreePPLAst in
  match parseTreePPLExn filename content with  file in
  
  use TreePPLCompile in
    let corePplAst: Expr = compile input file in
    --dprint corePplAst;
    --  TODO(vsenderov,2022-05-10): Maybe parse from the command line  
    let outName = "out.mc" in
    let options = { default with method = "mexpr-importance" } in
    --printLn (mexprPPLToString corePplAst);
    --let prog = corePplAst in
    let prog: Expr = typeCheck corePplAst in
    printLn "checkpoint 1";
    let ast = (mexprCompile options prog) in
    printLn "checkpoint 2";
    writeFile outName (use MExpr in concat "mexpr\n" (mexprToString ast));

    -- Output the compiled OCaml code (unless --skip-final is specified)
    let xx = if options.skipFinal then
      print (join ["Miking output written.\n", "To get an executable, compile with \n\n  mi compile ",outName, "\n\n"]);
      ()
    else sysRunCommand ["mi", "compile", outName] "" ".";
      print (join ["Executable compiled.\n", "To run \n\n  ./out \n\n"]);
      ()
    in
    use MExprPPL in print (concat (mexprPPLToString corePplAst) "\n\n"); ()    
