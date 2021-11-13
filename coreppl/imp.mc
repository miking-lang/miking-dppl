
include "ext/dist-ext.mc"
include "ext/math-ext.mc"
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


let printSave = lam res. lam names. lam normConst. lam expVals. lam varianceVals.
  let pad = 18 in
  let padPrint = lam s. lam n.
    if geqi n (length s) then
      print s; print (create (subi n (length s)) (lam. ' '))
    else print s in
  padPrint "Variable" 14;
  padPrint "Expected Value" pad;
  padPrint "Variance" pad;
  padPrint "Standard Deviation" pad;
  print "\n";
  recursive let work = lam names. lam ev. lam vv.
    match (names, ev, vv) with ([n]++ns, [e]++es, [v]++vs) then
      if isPrefix eqChar "#" n then work ns ev vv
      else
        padPrint n 14;
        padPrint (float2string e) pad;
        padPrint (float2string v) pad;
        padPrint (float2string (sqrt v)) pad;
        print "\n";
        work ns es vs
    else ()
  in
    work names expVals varianceVals;
    print "\n";
    print (join ["Normalization constant: ", float2string normConst, "\n"])

-- The output function. Prints normalizing constants, expected values, and variance
-- to the standard output. Saves the plot data in a CSV file.
let output = lam res. lam names.
  let nc = normConstant res in
  let expVals = expectedValues res nc in
  let varianceVals = variance res expVals in
  printSave res names nc expVals varianceVals

let printCSV = lam res. lam names.
  print (strJoin "," names); print "\n";
  iter (lam lst. print (strJoin "," (map float2string lst)); print "\n")
       (expOnLogWeights res)
