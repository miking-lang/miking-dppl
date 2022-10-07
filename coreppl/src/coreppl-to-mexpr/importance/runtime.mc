
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

let unwrapOpt : all a. Option a -> a = lam opt.
  match opt with Some x then x
  else error "Could not unwrap option"

-- General inference algorithm for importance sampling
let run : all a. (State -> a) -> ([Float], [a]) = lam model.

  -- Read number of runs
  match monteCarloArgs () with (particles, _) in

  let weightInit: Float = 0. in
  let states = createList particles (lam. ref weightInit) in
  let res = mapReverse model states in
  (mapReverse deref states, reverse (mapReverse unwrapOpt res))

let printRes : all a. (a -> String) -> ([Float], [a]) -> () = lam printFun. lam res.
  printLn (float2string (normConstant res.0));
  printSamples printFun res.0 res.1
