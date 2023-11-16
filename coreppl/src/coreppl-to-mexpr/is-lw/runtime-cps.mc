
include "common.mc"

include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

type Stop a
con Checkpoint : all a. { weight : Float, k : () -> Stop a } -> Stop a
con End : all a. a -> Stop a

-- In importance sampling, the state is simply the accumulated weight.
type State = Ref Float

let updateWeight = lam weight. lam k. Checkpoint { weight = weight, k = k }

let importance : all a. (State -> Stop a) -> State -> Option a =
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

let filterNone : all a. ([Float], [a]) -> Float -> Option a -> ([Float], [a]) =
  lam acc. lam weight. lam o.
  match o with Some v then
    match acc with (weightsAcc, resAcc) in
    (cons weight weightsAcc, cons v resAcc)
  else acc

-- General inference algorithm for importance sampling
let run : all a. Unknown -> (State -> Stop a) -> use RuntimeDistBase in Dist a = lam config. lam model.
  use RuntimeDist in

  let particles = config.particles in

  let weightInit: Float = 0. in
  let states = createList particles (lam. ref weightInit) in
  let res = mapReverse (importance model) states in
  let weights = mapReverse deref states in

  -- NOTE(dlunde,2022-10-27): It is very important that we compute the
  -- normalizing constant _before_ discarding the None values in filterNone.
  -- Otherwise the normalizing constant estimate is incorrect!
  let normConst = normConstant weights in

  match foldl2 filterNone ([], []) weights res with (weightsRev, resRev) in
  constructDistEmpirical resRev weightsRev
    (EmpNorm { normConst = normConst })
