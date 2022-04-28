
include "ocaml/mcore.mc"
include "seq.mc"

let logName = nameSym "externalLog"
let expName = nameSym "externalExp"
let log_ = lam x. app_ (nvar_ logName) x
let exp_ = lam x. app_ (nvar_ expName) x

let binomialSampleName = nameSym "externalBinomialSample"
let binomialLogPdfName = nameSym "externalBinomialLogPmf"
let binomialSample_ = lam p. lam n. app_ (app_ (nvar_ binomialSampleName) p) n
let bernoulliSample_ = lam p. app_ (app_ (nvar_ binomialSampleName) p) (int_ 1)
let bernoulliPmf_ = lam p. lam x. if_ x p (subf_ (float_ 1.) p)
let bernoulliLogPmf_ = lam p. lam x. log_ (bernoulliPmf_ p x)

let betaSampleName = nameSym "externalBetaSample"
let betaLogPdfName = nameSym "externalBetaLogPdf"
let betaSample_ = lam a. lam b. app_ (app_ (nvar_ betaSampleName) a) b
let betaLogPdf_ = lam a. lam b. lam x. app_ (app_ (app_ (nvar_ betaLogPdfName) x) a) b

let externalNames =[
  logName,
  expName,
  binomialSampleName,
  binomialLogPdfName,
  betaSampleName,
  betaLogPdfName
]


lang MExprExternalDists = MExprAst
  sem addExternalDists =
  | e ->
      let f = lam acc. lam n.
          TmExt {ident = n, tyIdent = tyunknown_,
                 effect = true, ty = tyunknown_,
                 inexpr = acc, info = NoInfo ()} in
      foldl f e externalNames
end
