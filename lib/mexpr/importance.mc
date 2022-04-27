
include "ocaml/mcore.mc"
include "seq.mc"
include "../coreppl/coreppl.mc"
include "../coreppl/dppl-arg.mc"
include "mexpr/ast-builder.mc"
include "external-dists.mc"

lang MExprPPLImportance = MExprPPL + MExprExternalDists

  sem transformImpSeq (accWeight:Name) =
  | TmAssume {dist = d} -> sampleDistExpr accWeight d
  | TmObserve {dist = d, value = v} ->  -- TODO: update log weight
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
      -- Add printing of accumulated weight and the result (one float)
      let e = bindall_ [
         nulet_ accWeight (ref_ (float_ 0.)),
         ulet_ "res" e,
         ulet_ "" (print_ (str_ "Result = ")),
         ulet_ "" (print_ (float2string_ (var_ "res"))),
         ulet_ "" (print_ (str_ "\nAccumulated weight = ")),
         ulet_ "" (print_ (float2string_ (deref_ (nvar_ accWeight)))),
         ulet_ "" (print_ (str_ "\n"))
      ] in
      -- Add imports of external distributions
      addExternalDists e
end



let importanceSamplingInference = lam options: Options. lam ast.
  use MExprPPLImportance in
  let ast = symbolize (transform ast) in
  -- Print (optional) the transformed MCore program
  if options.printMCore then
    printLn (mexprPPLToString ast);
    exit 0
  -- Execute the inference
  else
    let res = compileRunMCore "" [] ast in
    print res.stdout

