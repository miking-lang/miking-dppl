
include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

type Stop a
con Checkpoint : all a. { weight: Float, k: () -> Stop a } -> Stop a
con End : all a. a -> Stop a

-- In importance sampling, the state is simply the accumulated weight.
type State = Ref Float

let updateWeight = lam weight. lam k. Checkpoint { weight = weight, k = k }

let importance: all a. (State -> Stop a) -> State -> Option a =
  lam model. lam state.

    -- First run the initial model to the first checkpoint
    let res: Stop a = model state in

    -- Then run it until the end
    recursive let recEarlyStop: Stop a -> Option a = lam res.
      match res with Checkpoint { weight = weight, k = k } then
        modref state (addf (deref state) weight);
        if eqf (deref state) (negf inf) then None ()
        else recEarlyStop (k ())
      else match res with End a then Some a else never
    in

    recursive let rec: Stop a -> Option a = lam res.
      match res with Checkpoint { weight = weight, k = k } then
        modref state (addf (deref state) weight);
        rec (k ())
      else match res with End a then Some a else never
    in

    if compileOptions.earlyStop then recEarlyStop res
    else rec res

let unwrapOpt : all a. Option a -> a = lam opt.
  match opt with Some x then x
  else error "Could not unwrap option"

-- General inference algorithm for importance sampling
let run : all a. (State -> Stop a) -> ([Float], [a]) =
  lam model.

    -- Read number of runs and sweeps
    match monteCarloArgs () with (particles, sweeps) in

    let weightInit: Float = 0. in
    let states = createList particles (lam. ref weightInit) in
    let res = mapReverse (importance model) states in
    (mapReverse deref states, reverse (mapReverse unwrapOpt res))
