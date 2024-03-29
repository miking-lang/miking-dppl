include "common.mc"
include "ext/dist-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"

include "../runtime-common.mc"
include "../runtime-dists.mc"

type Checkpoint a
con Resample : all a. {k : () -> Checkpoint a} -> Checkpoint a
con End : all a. a -> Checkpoint a

type State = Ref Float

let resample = lam k. Resample {k = k}

let updateWeight = lam weight. lam state.
  modref state (addf (deref state) weight)

let stopFirstAssume = lam dist. lam cont. (Some dist, cont)
let stopInit = lam cont. (None (), cont)

-- WARNING: As of now, particles must be started and propagated sequentially (they cannot run in parallel)!
let run : all a. all b. Unknown
                 -> (State -> (Option (use RuntimeDistBase in Dist b), b -> Checkpoint a))
                 -> use RuntimeDistBase in Dist a =
  lam config. lam model.
  use RuntimeDist in

  let particleCount = config.particles in
  let logParticleCount = log (int2float particleCount) in

  let state = ref 0. in
  type Stop a = { weight: Float, checkpoint: Checkpoint a } in
  let start: (b -> Checkpoint a) -> Float -> (() -> b) -> Int -> Stop a =
    lam cont. lam weight. lam sampleFun. lam.
      modref state weight;
      let checkpoint: Checkpoint a = cont (sampleFun ()) in
      {
        weight = deref state,
        checkpoint = checkpoint
      }
  in
  let propagate = lam particle. lam contWeight.
    modref state contWeight;
    match particle.checkpoint with Resample {k = k} then
      let checkpoint = k () in
      {
        weight = deref state,
        checkpoint = checkpoint
      }
    else match particle.checkpoint with End _ then
      particle
    else never
  in
  recursive let runRec = lam particles.
    if forAll (lam p. match p.checkpoint with End _ then true else false) particles then
      unzip (mapReverse (lam p. (p.weight, match p.checkpoint with End a in a)) particles)
    else
      let maxWeight = foldl (lam acc. lam p. if geqf p.weight acc then p.weight else acc) (negf inf) particles in
      let expWeights = reverse (mapReverse (lam p. exp (subf p.weight maxWeight)) particles) in
      let sums = foldl (lam acc. lam w. (addf acc.0 w, addf acc.1 (mulf w w))) (0., 0.) expWeights in
      let ess = divf (mulf sums.0 sums.0) sums.1 in
      if ltf (mulf 0.7 (int2float particleCount)) ess then
        let particles = mapReverse (lam p. propagate p p.weight) particles in
        runRec particles
      else
        let contWeight = subf (addf maxWeight (log sums.0)) logParticleCount in
        let resampled = systematicSample particles expWeights sums.0 particleCount in
        let particles = mapReverse (lam p. propagate p contWeight) resampled in
        runRec particles
  in

  match model state with (d, cont) in
  let particles: [Stop a] =
    match d with Some d then
      use RuntimeDist in
      match d with DistEmpirical r then
        if eqi particleCount (length r.samples) then
          -- Call cont with sample = old sample for each particle, start at weight = old sample weight
          foldl2 (lam acc. lam s. lam lw. cons (start cont lw (lam. s) 0) acc)
            (toList []) r.samples r.logWeights
        else
          -- Call cont with sample = new sample for each particle, start at weight = 0
          createList particleCount (start cont 0. (lam. sample d))
      else
        -- Call cont with sample = new sample for each particle, start at weight = 0
        createList particleCount (start cont 0. (lam. sample d))
    else
      -- Call cont with sample = () for each particle, start at weight = 0
      createList particleCount (start cont 0. (lam. unsafeCoerce ()))
  in

  match runRec particles with (weights, samples) in

  -- Return
  if compileOptions.subsample then
    constructDistEmpiricalSubsample compileOptions.subsampleSize samples weights
      (EmpNorm {normConst = normConstant weights})
  else
  constructDistEmpirical samples weights
    (EmpNorm {normConst = normConstant weights})
