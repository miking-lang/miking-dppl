
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

-- Computing the normalization constant using the log-sum-exp trick
let normConstant = lam res.
  let negInf = (divf (negf 1.) 0.) in
  let max = foldl (lam acc. lam x.
                     let h = head x in
                     if geqf h acc then h else acc) negInf res in
  let sum = foldl (lam acc. lam x. addf (exp (subf (head x) max)) acc) 0. res in
  addf max (log sum)


-- Computes the expected value for all variables. Returns
-- a list that excludes the weight component and only contains
-- the expected values for the given variables
let expectedValues = lam res. lam normConst.
  foldl (lam acc. lam t.
     let w = exp (subf (head t) normConst) in
     let ys = tail t in
     recursive let work = lam acc. lam xs.
       match (acc,xs) with ([a]++as, [x]++xs) then
         cons (addf (mulf x w) a) (work as xs)
       else []
     in
       work acc ys) (create (subi (length (head res)) 1) (lam. 0.)) res

-- Computes the variances for the list of variables
let variance = lam res. lam expVals.
  let sum = foldl (lam acc. lam t.
    recursive let work = lam acc. lam xs. lam expv.
      match (acc,xs,expv) with ([a]++as, [x]++xs, [e]++es) then
        let v = subf x e in
        cons (addf a (mulf v v)) (work as xs es)
      else []
    in
      work acc (tail t) expVals) (create (subi (length (head res)) 1) (lam. 0.)) res
  in
    let dval = int2float (length res) in
    map (lam x. divf x dval) sum



-- The output function. Prints normalizing constants, expected values, and variance
-- to the standard output. Saves the plot data in a CSV file.
let output = lam res. lam names.
  let names = cons "#" names in
  let nc = normConstant res in
  let expVals = expectedValues res nc in
  let varianceVals = variance res expVals in
  printStatistics res names nc expVals varianceVals;
  saveCSV res names "data.csv" expOnLogWeights
