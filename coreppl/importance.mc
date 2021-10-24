

-- include "ocaml/compile.mc"
include "compile-mcore.mc"
include "seq.mc"
include "coreppl.mc"

lang MExprPPLImportance = MExprPPL

  sem transformImpSeq =
  | TmAssume {dist = d} -> sampleDistExpr d
  | TmObserve {dist = d, value = v} -> logPdfDistExpr v d
  | expr -> smap_Expr_Expr transformImpSeq expr

  sem sampleDistExpr =
  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (var_ "betaSample") a) b)
  | TmDist { dist = DBernoulli {p = p}} -> (app_ (var_ "bernoulliSample") p)

  sem logPdfDistExpr (x:Expr) =
  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (app_ (var_ "betaLogPdf") a) b) x)
  | TmDist { dist = DBernoulli {p = p}} -> (app_ (app_ (var_ "bernoulliLogPmf") p) x)

end



let importanceSamplingInference = lam options. lam ast.
  use MExprPPLImportance in
  let ast = transformImpSeq ast in
  -- Print (optional) the transformed MCore program
  (if options.printMCore then
    printLn (expr2str ast)
  else ());
  -- Compile and run the code
  compileRunMCore ast
