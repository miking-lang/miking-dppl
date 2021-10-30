
include "ocaml/mcore.mc"
include "seq.mc"
include "coreppl.mc"
include "dppl-arg.mc"
include "external-dists.mc"

lang MExprPPLImportance = MExprPPL + MExprExternalDists

  sem transformImpSeq (stateName:Name) =
  | TmAssume {dist = d} -> sampleDistExpr stateName d
  | TmObserve {dist = d, value = v} -> logPdfDistExpr v stateName d
  | expr -> smap_Expr_Expr (transformImpSeq stateName) expr

  sem sampleDistExpr (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> betaSample_ a b
  | TmDist { dist = DBernoulli {p = p}} -> bernoulliSample_ p

  sem logPdfDistExpr (x:Expr) (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> betaLogPdf_ a b x
  | TmDist { dist = DBernoulli {p = p}} -> bernoulliLogPmf_ p x

  sem transform =
  | e ->
      -- Create a name for the accumulated weight
      let stateName = nameSym "accWeight" in
      -- Transform the AST, rewrite with importance sampling
      let e = transformImpSeq stateName e in
      -- Add the state on top
      let e = bind_ (nulet_ stateName (ref_ (float_ 0.))) e in
      -- Add imports of external distributions
      addExternalDists e
end



let importanceSamplingInference = lam options: Options. lam ast.
  use MExprPPLImportance in
  let ast = symbolize (transform ast) in
  -- Print (optional) the transformed MCore program
  if options.printMCore then
    printLn (expr2str ast)
  -- Execute the inference
  else
    let res = compileRunMCore "" [] ast in
    print res.stdout
