
include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"

include "../runtime/stats.mc"
include "../runtime/dists.mc"

-- In importance sampling, the state is simply the accumulated weight.
type State = Ref Float

let updateWeight = lam v. lam state.
  modref state (addf (deref state) v)

-- General inference algorithm for importance sampling
let run = lam model.
  let particles = 1000 in
  let weightInit = log (divf 1. (int2float particles)) in
  let states = createList particles (lam. ref weightInit) in
  let res = mapReverse model states in
  let res = (mapReverse deref states, res) in
  res

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
