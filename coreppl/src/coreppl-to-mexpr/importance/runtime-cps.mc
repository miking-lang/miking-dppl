
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

let filterNone : all a. ([Float], [a]) -> Ref Float -> Option a -> ([Float], [a]) =
  lam acc. lam state. lam res.
  match res with Some v then
    match acc with (weightsAcc, resAcc) in
    (cons (deref state) weightsAcc, cons v resAcc)
  else acc

-- General inference algorithm for importance sampling
let run : all a. (State -> Stop a) -> ([Float], [a]) =
  lam model.

    -- Read number of runs and sweeps
    match monteCarloArgs () with (particles, sweeps) in

    let weightInit: Float = 0. in
    let states = createList particles (lam. ref weightInit) in
    let res = mapReverse (importance model) states in

    match foldl2 filterNone ([], []) states res with (weightsRev, resRev) in
    (weightsRev, reverse resRev)

let printRes : all a. (a -> String) -> ([Float], [a]) -> () = lam printFun. lam res.
  printLn (float2string (normConstant res.0));
  printSamples printFun res.0 res.1
