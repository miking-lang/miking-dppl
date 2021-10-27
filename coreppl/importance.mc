
include "ocaml/mcore.mc"
include "seq.mc"
include "coreppl.mc"
include "dppl-arg.mc"

lang MExprPPLImportance = MExprPPL

  sem transformImpSeq (stateName:Name) =
  | TmAssume {dist = d} -> sampleDistExpr stateName d
  | TmObserve {dist = d, value = v} -> logPdfDistExpr v stateName d
  | expr -> smap_Expr_Expr (transformImpSeq stateName) expr

  sem sampleDistExpr (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (var_ "betaSample") a) b)
  | TmDist { dist = DBernoulli {p = p}} -> (app_ (var_ "bernoulliSample") p)

  sem logPdfDistExpr (x:Expr) (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (app_ (var_ "betaLogPdf") a) b) x)
  | TmDist { dist = DBernoulli {p = p}} -> (app_ (app_ (var_ "bernoulliLogPmf") p) x)

  sem addState =
  | e -> let stateName = nameSym "accWeight" in
         {name = stateName, expr = bind_ (nulet_ stateName (ref_ (float_ 0.))) e}

end



let importanceSamplingInference = lam options: Options. lam ast.
  use MExprPPLImportance in
  let res = addState ast in
  let ast = transformImpSeq res.name res.expr in
  let ast = symbolize ast in
  -- Print (optional) the transformed MCore program
  (if options.printMCore then
    printLn (expr2str ast)
  else ());
  let res = compileRunMCore "" [] ast in
  print res.stdout
