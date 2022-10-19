
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

let filterNone : all a. ([Float], [a]) -> Ref Float -> Option a -> ([Float], [a]) =
  lam acc. lam state. lam o.
  match o with Some v then
    match acc with (weightsAcc, resAcc) in
    (cons (deref state) weightsAcc, cons v resAcc)
  else acc

-- General inference algorithm for importance sampling
let run : all a. Unknown -> (State -> Stop a) -> Dist a = lam config. lam model.
  use RuntimeDist in

  let particles = config.particles in

  let weightInit: Float = 0. in
  let states = createList particles (lam. ref weightInit) in
  let res = mapReverse (importance model) states in

  match foldl2 filterNone ([], []) states res with (weightsRev, resRev) in
  DistEmpirical {
    logWeights = weightsRev,
    samples = reverse resRev,

    -- TODO(dlunde,2022-10-19): Properly extract the normalizing constant
    extra = EmpNorm { normConst = negf 1.0 }
  }
