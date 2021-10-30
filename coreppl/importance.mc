
include "ocaml/mcore.mc"
include "seq.mc"
include "coreppl.mc"
include "dppl-arg.mc"

--let binomialSample = lam p:Float. lam n:Int. externalBinomialSample p n

let myName = nameSym "externalBetaSample"

lang MExprPPLImportance = MExprPPL

  sem transformImpSeq (stateName:Name) =
  | TmAssume {dist = d} -> sampleDistExpr stateName d
  | TmObserve {dist = d, value = v} -> logPdfDistExpr v stateName d
  | expr -> smap_Expr_Expr (transformImpSeq stateName) expr

  sem sampleDistExpr (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (nvar_ myName) a) b)
--  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (var_ "betaSample") a) b)
  | TmDist { dist = DBernoulli {p = p}} -> (app_ (var_ "bernoulliSample") p)

  sem logPdfDistExpr (x:Expr) (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (app_ (var_ "betaLogPdf") a) b) x)
  | TmDist { dist = DBernoulli {p = p}} -> (app_ (app_ (var_ "bernoulliLogPmf") p) x)

  sem addState =
  | e -> let stateName = nameSym "accWeight" in
      let e_in = bind_ (nulet_ stateName (ref_ (float_ 0.))) e in
      let e2 = TmExt {ident = myName, tyIdent = tyunknown_, effect = true, ty = tyunknown_,
         inexpr = e_in, info = NoInfo ()} in
      {name = stateName, expr = e2}
--         {name = stateName, expr = bind_ (nulet_ stateName (ref_ (float_ 0.))) e}
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
