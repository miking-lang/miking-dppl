
include "ext/dist-ext.mc"
include "seq.mc"
include "string.mc"

-- In importance sampling, the state is simply the accumulate weight.
type State = Float

-- Updates the weight in the state
let updateWeight = lam v. lam state.
  modref state (addf (deref state) v)

let observeBernoulli = lam v. lam p. lam state.
  updateWeight (bernoulliLogPmf p (if v then 1 else 0)) state

let observeBeta = lam v. lam a. lam b. lam state.
  updateWeight (betaLogPdf a b v) state

let assumeBernoulli = lam p. lam state.
  bernoulliSample p

let assumeBeta = lam a. lam b. lam state.
  betaSample a b

let inferImp = lam particles. lam model.
  let states = createList particles (lam. ref (log (divf 1. (int2float particles)))) in
  let res = mapReverse model states in
  mapReverse (lam t. match t with (xs, w) in cons (deref w) xs) res

-- The log-weight is always in the first column (list of lists)
let expOnLogWeights = lam res.
  mapReverse (lam t. match t with [x]++xs in cons (exp x) xs) res

-- Straightforward (inefficient) computation of the normalizing constant
let normConstantInefficient = lam res.
  log (foldl (lam acc. lam x. addf (exp (head x)) acc) 0. res)

-- The output function. Prints normalizing constants, expected values, and variance
-- to the standard output. Saves the plot data in a CSV file.
let output = lam res. lam names.
  let nc = normConstantInefficient res in
  print (join ["Normalization constant: ", float2string nc, "\n"])

let printCSV = lam res. lam names.
  print (strJoin "," names); print "\n";
  iter (lam lst. print (strJoin "," (map float2string lst)); print "\n")
       (expOnLogWeights res)
