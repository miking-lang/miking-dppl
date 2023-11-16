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

let run : all a. Unknown -> (State -> Checkpoint a) -> use RuntimeDistBase in Dist a =
  lam config. lam model.
  use RuntimeDist in

  --------------
  -- SMC PART --
  --------------

  -- Internal SMC Algorithm, for now the bootstrap particle filter.

  -- TODO(oerikss, 2022-11-01): We could parametrize this algorithm by its
  -- internal SMC algorithm.

  -- WARNING: As of now, particles must be started and propagated sequentially
  -- (they cannot run in parallel)!

  let particleCount = config.particles in
  let logParticleCount = log (int2float particleCount) in

  -- print "N-PARTICLES: "; printLn (int2string (particleCount));

  let runSMC : () -> ([Float], [a]) = lam.
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
      switch particle.checkpoint
      case Resample {k = k} then
        let checkpoint = k () in
        {
          weight = deref state,
          checkpoint = checkpoint
        }
      case End _ then particle
      end
    in
    recursive let runRec = lam particles.
      if forAll
          (lam p. match p.checkpoint with End _ then true else false) particles
      then
        unzip
          (mapReverse (lam p. (p.weight, match p.checkpoint with End a in a))
            particles)
      else
        -- print "RESAMPLING"; printLn "";
        let maxWeight =
          foldl
            (lam acc. lam p. if geqf p.weight acc then p.weight else acc)
            (negf inf)
            particles
        in
        let expWeights = map (lam p. exp (subf p.weight maxWeight)) particles in
        let sums =
          foldl
            (lam acc. lam w. (addf acc.0 w, addf acc.1 (mulf w w)))
            (0., 0.)
            expWeights
        in
        let ess = divf (mulf sums.0 sums.0) sums.1 in
        if ltf (mulf 0.7 (int2float particleCount)) ess then
          let particles = mapReverse (lam p. propagate p p.weight) particles in
          runRec particles
        else
          let contWeight =
            subf (addf maxWeight (log sums.0)) logParticleCount
          in
          let resampled =
            systematicSample particles expWeights sums.0 particleCount
          in
          let particles = mapReverse (lam p. propagate p contWeight) resampled in
          runRec particles
    in
    let particles = createList particleCount start in
    runRec particles
  in

  -------------
  -- MH PART --
  -------------

  -- Metropolis Hastings iteration
  recursive let mh =
    lam logZs. lam weightSets. lam sampleSets. lam iter.
      if leqi iter 0 then (logZs, weightSets, sampleSets)
      else
        -- Compute a new proposal with a particle filter sweep
        match runSMC () with (weights, samples) in
        -- print "N-PARTIVLES: "; printLn (int2string (length weights));
        -- print "WEIGHT-PARTICLE-1: "; printLn (float2string (head weights));
        let logZ = normConstant weights in
        -- print "LOG-Z: "; printLn (float2string logZ);
        let prevLogZ = head logZs in
        let prevSamples = head sampleSets in
        let prevWeights = head weightSets in
        let logMhAcceptProb = minf 0. (subf logZ prevLogZ) in
        let iter = subi iter 1 in
        if bernoulliSample (exp logMhAcceptProb) then
          mcmcAccept ();
          mh
            (cons logZ logZs)
            (cons weights weightSets)
            (cons samples sampleSets)
            iter
        else
          mh
            (cons prevLogZ logZs)
            (cons prevWeights weightSets)
            (cons prevSamples sampleSets)
            iter
  in

  -- Initialization
  let runs = config.iterations in

  -- Used to keep track of acceptance ratio
  mcmcAcceptInit runs;

  -- Initial sample
  match runSMC () with (weights, samples) in
  let iter = subi runs 1 in

  -- print "N-WEIGHTS: "; printLn (int2string (length weights));

  -- Draw remaining samples
  match mh [normConstant weights] [weights] [samples] iter with
    (logZs, weightSets, sampleSets)
  in

  -- Reverse to get the correct order
  let logZs = reverse logZs in
  let weightSets = reverse weightSets in
  let sampleSets = reverse sampleSets in

  -- flatten and normalize weights
  let weights =
    join (
      zipWith
        (lam logWs. lam logZ.
          if eqf logZ negInf then (create particleCount (lam. negInf))
          else  map (lam logW. subf logW logZ) logWs)
        weightSets logZs)
  in
  let samples = join sampleSets in

  -- Return
  constructDistEmpirical samples weights
    (EmpMCMC { acceptRate = mcmcAcceptRate () })
