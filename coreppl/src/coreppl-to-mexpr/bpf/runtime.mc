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

-- WARNING: As of now, particles must be started and propagated sequentially (they cannot run in parallel)!
let run : all a. Unknown -> (State -> Checkpoint a) -> Dist a =
  lam config. lam model.
  use RuntimeDist in

  let particleCount = config.particles in
  let logParticleCount = log (int2float particleCount) in

  let state = ref 0. in
  let start = lam.
    modref state 0.;
    let checkpoint = model state in
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
      let expWeights = map (lam p. exp (subf p.weight maxWeight)) particles in
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
  let particles = createList particleCount start in
  match runRec particles with (weights, samples) in
  DistEmpirical {
    weights = weights,
    samples = samples,
    extra = EmpNorm {normConst = negf 1.0}
  }
