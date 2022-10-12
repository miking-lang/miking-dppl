
include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

-- In importance sampling, the state is simply the accumulated weight.
type State = Ref Float

let updateWeight = lam v. lam state. modref state (addf (deref state) v)

let unwrapOpt : all a. Option a -> a = lam o.
  match o with Some x then x
  else error "Could not unwrap option"

-- General inference algorithm for importance sampling
let run : all a. (State -> a) -> Dist a = lam model.
  use RuntimeDist in

  -- Read number of runs
  match monteCarloArgs () with (particles, _) in

  let weightInit = 0.0 in
  let states = createList particles (lam. ref weightInit) in
  let res = mapReverse model states in
  DistImportance { weights = mapReverse deref states
                 , samples = res }
