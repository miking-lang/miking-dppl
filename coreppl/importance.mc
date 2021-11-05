
include "ocaml/mcore.mc"
include "seq.mc"
include "coreppl.mc"
include "dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "external-dists.mc"


lang MExprPPLImportance = MExprPPL + MExprExternalDists

  sem transformImpSeq (accWeight:Name) =
  | TmAssume {dist = d} -> sampleDistExpr accWeight d
  | TmObserve {dist = d, value = v} ->
     modref_ (nvar_ accWeight) (logPdfDistExpr v accWeight d)
  | expr -> smap_Expr_Expr (transformImpSeq accWeight) expr

  sem sampleDistExpr (accWeight:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> betaSample_ a b
  | TmDist { dist = DBernoulli {p = p}} -> bernoulliSample_ p

  sem logPdfDistExpr (x:Expr) (accWeight:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> betaLogPdf_ a b x
  | TmDist { dist = DBernoulli {p = p}} -> bernoulliLogPmf_ p x

  sem transform =
  | e ->
      -- Create a name for the accumulated weight
      let accWeight = nameSym "accWeight" in
      -- Transform the AST, rewrite with importance sampling
      let e = transformImpSeq accWeight e in
      -- Add the state on top
      let e = symlet_ accWeight (ref_ (float_ 0.)) e in
      -- Add imports of external distributions
      addExternalDists e
      -- Add printing of accumulated weight
end



let importanceSamplingInference = lam options: Options. lam ast.
  use MExprPPLImportance in
  let ast =  (transform ast) in
  -- Print (optional) the transformed MCore program
  if options.printMCore then
    printLn (expr2str ast);
    exit 0
  -- Execute the inference
  else
    let res = compileRunMCore "" [] ast in
    print res.stdout

mexpr

utest
  use MExprPPLImportance in
  let x = transform (observe_ (float_ 0.1) (beta_ (float_ 2.) (float_ 2.))) in
  print "\n------\n";
  print (expr2str x);
  print "\n------\n";
  int_ 0
with int_ 0 in

()
