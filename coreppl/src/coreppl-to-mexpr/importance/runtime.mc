
include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"

include "../runtime/common.mc"
include "../runtime/dists.mc"

-- In importance sampling, the state is simply the accumulated weight.
type State = Ref Float

let updateWeight = lam v. lam state. modref state (addf (deref state) v)

-- General inference algorithm for importance sampling
let run : all a. (State -> a) -> (Res a -> ()) -> () = lam model. lam printResFun.

  -- Read number of runs and sweeps
  match monteCarloArgs () with (particles, sweeps) in

  -- Repeat once for each sweep
  repeat (lam.
      let weightInit: Float = 0. in
      let states = createList particles (lam. ref weightInit) in
      let res = mapReverse model states in
      let res = (mapReverse deref states, res) in
      printResFun res
    ) sweeps

let printRes : all a. (a -> String) -> Res a -> () = lam printFun. lam res.
  printLn (float2string (normConstant res.0));
  if compileOptions.printSamples then
    printSamples printFun res.0 res.1
  else ()
