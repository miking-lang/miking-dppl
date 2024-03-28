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
let run : all a. Unknown -> (State -> Checkpoint a) -> use RuntimeDistBase in Dist a =
  lam config. lam model.
  use RuntimeDist in

  let particleCount = config.particles in
  let logParticleCount = log (int2float particleCount) in
  let negInf = negf inf in

  let state = ref 0. in
  let start = lam.
    recursive let startRec = lam index. lam propagations.
      modref state 0.;
      let checkpoint = model state in
      if eqf (deref state) negInf then
        startRec index (addi propagations 1)
      else
        {
          weight = if eqi index 0 then negInf else deref state,
          checkpoint = checkpoint,
          propagations = addi propagations 1
        }
    in
    createList particleCount (lam i. startRec i 0)
  in
  let propagate = lam particles.
    let maxWeight = foldl (lam acc. lam p. if geqf p.weight acc then p.weight else acc) negInf particles in
    let expWeights = reverse (mapReverse (lam p. exp (subf p.weight maxWeight)) particles) in
    let expWeightSum = foldl (lam acc. lam w. (addf acc w)) 0. expWeights in
    let propagations = foldl (lam acc. lam p. (addi acc p.propagations)) 0 particles in
    let contWeight = subf (addf maxWeight (log expWeightSum)) (log (int2float (subi propagations 1))) in
    let dropIndex = randIntU 0 particleCount in
    recursive let propagateRec = lam index. lam propagations. lam out.
      recursive let propagateRecRec = lam index. lam particles. lam out.
        if null particles then
          if eqi index particleCount then
            out
          else
            propagateRec index (addi propagations 1) out
        else
          let particle = head particles in
          modref state contWeight;
          match particle.checkpoint with Resample {k = k} then
            let checkpoint = k () in
            if eqf (deref state) negInf then
              propagateRecRec index (tail particles) out
            else
              propagateRecRec (addi index 1) (tail particles) (cons {
                weight = if eqi index dropIndex then negInf else deref state,
                checkpoint = checkpoint,
                propagations = addi propagations 1
              } out)
          else match particle.checkpoint with End _ then
            propagateRecRec (addi index 1) (tail particles) (cons {
              weight = if eqi index dropIndex then negInf else particle.weight,
              checkpoint = particle.checkpoint,
              propagations = addi propagations 1
            } out)
          else never
      in
      let resampled = systematicSample particles expWeights expWeightSum (subi particleCount index) in
      propagateRecRec index resampled out
    in
    propagateRec 0 0 (toList [])
  in
  recursive let runRec = lam particles.
    if forAll (lam p. match p.checkpoint with End _ then true else false) particles then
      let propagations = (foldl (lam acc. lam p. (addi acc p.propagations)) 0 particles) in
      let corrFactor = subf logParticleCount (log (int2float (subi propagations 1))) in
      unzip (mapReverse (lam p. (addf p.weight corrFactor, match p.checkpoint with End a in a)) particles)
    else
      runRec (propagate particles)
  in
  let particles = start () in
  match runRec particles with (weights,samples) in

  -- Return
  if compileOptions.subsample then
    constructDistEmpiricalSubsample compileOptions.subsampleSize samples weights
      (EmpNorm {normConst = normConstant weights})
  else
    constructDistEmpirical samples weights
      (EmpNorm {normConst = normConstant weights})
