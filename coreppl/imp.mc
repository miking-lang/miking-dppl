
include "ext/dist-ext.mc"
include "seq.mc"
include "string.mc"

-- In importance sampling, the state is simply the accumulate weight.
type State = Float

let mapReverse = lam f. lam lst.
  foldl (lam acc. lam x. cons (f x) acc) [] lst

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
  let states = createList particles (lam. ref 0.) in
  let res = mapReverse model states in
  mapReverse (lam t. match t with ([x], w) in [x, exp (deref w)]) res


let printCSV = lam res. lam names.
  print (strJoin "," names); print "\n";
  iter (lam lst. print (strJoin "," (map float2string lst)); print "\n") res


