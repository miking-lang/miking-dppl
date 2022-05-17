/- 
TreePPL Compiler
Authors: Viktor S and Viktor P
Dates: 2022

Hello!

This is the TreePPL compiler (part of miking-dppl).

Here is the compilation pipeline:

example.tppl 
  --parse--> (part of Viktor's Tool)
TreePPL AST 
  --analysis--> (part of this compiler)
Side effects and enriched TreePPL AST 
  --compile TPPL--> (part of this compiler)
CorePPL AST + Variant Record Projection (another syn Expr...) 
  --type checker--> 
CorePPL AST + Variant Record Projection with types 
  --desugar--> 
CorePPL AST 
  --CorePPL compiler-->
 RootPPL CU file 
  --rootppl-->
RootPPL program 

To run this compiler (when it's ready), try:

  mi compile --typecheck --keep-dead-code compiler.mc
  mi compile --keep-dead-code compiler.mc
  boot eval compiler

TODO try entr to autocompile on save.

Some inspiration is drawn from the CorePPL C compiler.
-/

--include --TODO pretty printing facilities
include "treeppl-ast.mc"

mexpr

match argv with ![_, _] then
  printLn "----Please provide exactly two arguments; a .syn file and the output .mc file";
  exit 0
else
match argv with [_, filename] in
let content = readFile filename in
use TreePPLAst in
match parseTreePPLExn filename content with x in
();
-- match file.decl with file in ();
dprint x


