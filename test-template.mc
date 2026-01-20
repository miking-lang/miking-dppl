include "coreppl::coreppl-to-mexpr/pval-graph/simple-mcmc.mc"
include "coreppl::coreppl-to-mexpr/pval-graph/pval-mut.mc"
include "coreppl::coreppl-to-mexpr/pval-graph/pval-debug.mc"
include "ext/mat-ext.mc"
include "common.mc"

-- NOTE(vipa, 2025-12-09): In lieu of proper distribution translations
-- I'll make these easy to substitute in
let mkBernoulli = lam p. use RuntimeDistElementary in DistBernoulli {p = p}
let mkExponential = lam rate. use RuntimeDistElementary in DistExponential {rate = rate}
let mkGaussian = lam mu. lam sigma. use RuntimeDistElementary in DistGaussian {mu = mu, sigma = sigma}
let mkGamma = lam shape. lam scale. use RuntimeDistElementary in DistGamma {shape = shape, scale = scale}
let mkPoisson = lam lambda. use RuntimeDistElementary in DistPoisson {lambda = lambda}
let mkUniform = lam a. lam b. use RuntimeDistElementary in DistUniform {a = a, b = b}
let mkDirichlet = lam a. use RuntimeDistElementary in DistDirichlet {a = a}
let mkCategorical = lam p. use RuntimeDistElementary in DistCategorical {p = p}

lang Model = SimpleMCMCPVal
  sem model = | st ->
    (
{{HERE}}
    ) st
end

lang ComposedMut = Model + MutPVal + RuntimeDistElementary
end

lang ComposedVisi = Model + PValVisiGraph + RuntimeDistElementary
end

let timeF : all a. (() -> a) -> (Float, a)
  = lam f.
    let before = wallTimeMs () in
    let res = f () in
    let after = wallTimeMs () in
    (subf after before, res)

let interval2string : (Float, Float) -> String
  = lam pair.
    join [float2string pair.0, "-", float2string pair.1]

let histogram : all a. (a -> a -> Int) -> [a] -> [(a, Float)]
  = lam cmp. lam l.
    let hist = foldl (lam acc. lam a. mapInsertWith addi a 1 acc) (mapEmpty cmp) l in
    let count = int2float (mapFoldWithKey (lam total. lam. lam count. addi total count) 0 hist) in
    let hist = mapMap (lam v. divf (int2float v) count) hist in
    mapBindings hist

let bucket : all a. Int -> Float -> Float -> [Float] -> [((Float, Float), Float)]
  = lam numBuckets. lam min. lam max. lam l.
    let bucketSize = divf (subf max min) (int2float numBuckets) in
    let hist = mapFromSeq subi (create numBuckets (lam i. (i, 0))) in
    let f = lam acc. lam x. mapInsertWith addi (floorfi (divf (subf x min) bucketSize)) 1 acc in
    let hist = foldl f hist l in
    let count = int2float (mapFoldWithKey (lam total. lam. lam count. addi total count) 0 hist) in
    let convPair = lam pair.
      let base = addf min (mulf bucketSize (int2float pair.0)) in
      ( (base, addf bucketSize base)
      , divf (int2float pair.1) count
      ) in
    map convPair (mapBindings hist)

let progressBarNoPad : Int -> Float -> String
  = lam width. lam fraction.
    let filled = roundfi (mulf (int2float width) fraction) in
    make filled '=' -- (make (subi width filled) ' ')

let hist2string : all a. (a -> String) -> [(a, Float)] -> String
  = lam toStr. lam l.
    strJoin "\n" (map (lam pair. join [toStr pair.0, "\t", float2string pair.1, "\t", progressBarNoPad 100 pair.1]) l)

mexpr

let showHistogram : Bool = true in

let globalProb = 1.0 in
let iterations = 1000 in
let toString = lam. "()" in
let mkHisto = histogram (lam. lam. 0) in
-- let toString = interval2string in
-- let mkHisto = bucket 10 0.0 1. in
let summarizePVal = lam label. lam pair.
  match pair with (time, res) in
  printLn (join [float2string time, "ms (", label, ")"]);
  printLn (float2string res.acceptanceRatio);
  if showHistogram then printLn (hist2string toString (mkHisto res.samples)) else () in
let run =
  use ComposedVisi in
  -- printJsonLn (graphToJson (instantiate model (simpleInit ())));
  -- exit 0;
  use ComposedMut in
  recursive let findGoodInstance = lam.
    let instance = instantiate model (simpleInit ()) in
    if eqf (getWeight instance) (negf inf)
    then findGoodInstance ()
    else instance in
  match timeF findGoodInstance with (time, instance) in
  match getSt instance with SimpleState st in
  printLn (join [int2string (length st.here), ", ", int2string (length st.below)]);
  printLn (join ["Took ", float2string time, "ms to find good instance."]);
  lam. mcmc (mkMCMCConfig iterations globalProb) instance in
summarizePVal "mut" (timeF run);

()
