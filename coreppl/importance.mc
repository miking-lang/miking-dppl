
include "ocaml/mcore.mc"
include "seq.mc"
include "coreppl.mc"
include "dppl-arg.mc"

let betaSampleName = nameSym "externalBetaSample"
let betaSample = lam a. lam b. app_ (app_ (nvar_ betaSampleName) a) b

let binomialSampleName = nameSym "externalBinomialSample"
let binomialSample = lam p. lam b. app_ (app_ (nvar_ binomialSampleName) p) b
let bernoulliSample = lam p. app_ (app_ (nvar_ binomialSampleName) p) (int_ 1)


let externalNames =[
  betaSampleName,
  binomialSampleName
]

lang MExprExternalDists = MExprAst
   sem addExternalDists =
  | e ->
      let f = lam acc. lam n. TmExt {ident = n, tyIdent = tyunknown_,
                       effect = true, ty = tyunknown_, inexpr = acc, info = NoInfo ()} in
      foldl f e externalNames
end

lang MExprPPLImportance = MExprPPL + MExprExternalDists

  sem transformImpSeq (stateName:Name) =
  | TmAssume {dist = d} -> sampleDistExpr stateName d
  | TmObserve {dist = d, value = v} -> logPdfDistExpr v stateName d
  | expr -> smap_Expr_Expr (transformImpSeq stateName) expr

  sem sampleDistExpr (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> betaSample a b
  | TmDist { dist = DBernoulli {p = p}} -> bernoulliSample p
--  | TmDist { dist = DBernoulli {p = p}} -> (app_ (var_ "bernoulliSample") p)

  sem logPdfDistExpr (x:Expr) (stateName:Name) =
  | TmDist { dist = DBeta {a = a, b = b}} -> (app_ (app_ (app_ (var_ "betaLogPdf") a) b) x)
  | TmDist { dist = DBernoulli {p = p}} -> (app_ (app_ (var_ "bernoulliLogPmf") p) x)

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
--      let e2 = TmExt {ident = betaSampleName, tyIdent = tyunknown_, effect = true, ty = tyunknown_,
--         inexpr = e_in, info = NoInfo ()} in
--       let e2 = e_in in
--      {name = stateName, expr = e2}
--         {name = stateName, expr = bind_ (nulet_ stateName (ref_ (float_ 0.))) e}
end



let importanceSamplingInference = lam options: Options. lam ast.
  use MExprPPLImportance in
  let ast = transform ast in
  let ast = symbolize ast in
  -- Print (optional) the transformed MCore program
  if options.printMCore then
    printLn (expr2str ast)
  -- Execute the inference
  else
    let res = compileRunMCore "" [] ast in
    print res.stdout
