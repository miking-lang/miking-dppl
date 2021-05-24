

include "mexpr/boot-parser.mc"



lang DPPLParser = BootParser + MExprPrettyPrint


end



let getAst = lam filename. lam printModel.
  use DPPLParser in
  -- Read and parse the mcore file
  let ast = parseMCoreFile [] filename in
  -- Pretty print the model?
  if printModel then
    print (expr2str ast);
    print "\n"
  else ();
  ast
