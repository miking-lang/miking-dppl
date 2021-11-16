
include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"
include "inference-common.mc"

-- In importance sampling, the state is simply the accumulate weight.
type State = Ref Float

-- Updates the weight in the state
let updateWeight = lam v. lam state.
  modref state (addf (deref state) v)

let observeBernoulli = lam v. lam p. lam state:State.
  updateWeight (bernoulliLogPmf p (if v then 1 else 0)) state

let observeBeta = lam v. lam a. lam b. lam state:State.
  updateWeight (betaLogPdf a b v) state

let assumeBernoulli = lam p. lam state:State.
  bernoulliSample p

let assumeBeta = lam a. lam b. lam state:State.
  betaSample a b

-- General inference algorithm for importance sampling
let infer = lam particles. lam model.
  let states = createList particles (lam. ref (log (divf 1. (int2float particles)))) in
  let res = mapReverse model states in
  mapReverse (lam t. match t with (xs, w) in cons (deref w) xs) res

-- The log-weight is always in the first column (list of lists)
let expOnLogWeights = lam res.
  mapReverse (lam t. match t with [x]++xs in cons (exp x) xs) res


-- The output function. Prints normalizing constants, expected values, and variance
-- to the standard output. Saves the plot data in a CSV file.
let output = lam res. lam names.
  let names = cons "#" names in
  let nc = normConstant res in
  let expVals = expectedValues res nc in
  let varianceVals = variance res expVals in
  printStatistics res names nc expVals varianceVals;
  saveCSV res names "data.csv" expOnLogWeights
