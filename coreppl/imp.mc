
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

let inferImportance = lam particles. lam model.
  let states = create particles (lam. ref 0.) in
  let vals = map model states in
   {weights = map (lam x. deref x) states, values = vals}

let infer = inferImportance 10

let model = lam state:State.
  let x = assumeBeta 10.0 8.0 state in
  observeBernoulli true x state;
  observeBernoulli true x state;
  [x]

mexpr
let s = ref 0. in
match model s with [x] in

print (join ["Accumulated weight: ", float2string (deref s), "\n"]);
print (join ["Result: ", float2string x, "\n"]);
let res = infer model in
print "\nWeights: \n";
iter (lam x. print (float2string x); print "\n") res.weights;
print "\nValues: \n";
iter (lam v. match v with [x] in print (float2string x); print "\n") res.values
