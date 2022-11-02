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

let run =
  lam config. lam model.
  use RuntimeDist in

  let lse = lam xs.
    let maxx =
      foldl (lam acc. lam x. if geqf x acc then x else acc) (negf inf) xs
    in
    addf maxx (log (foldl (lam acc. lam x. addf acc (exp (subf x maxx))) 0. xs))
  in

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

  let runSMC = lam.
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
    recursive let runRec = lam t.
      match t with (particles, evidence) in
      if forAll
          (lam p. match p.checkpoint with End _ then true else false) particles
      then
        (unzip
          (mapReverse (lam p. (p.weight, match p.checkpoint with End a in a))
            particles),
        evidence)
      else
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
          runRec (particles, evidence)
        else
          let contWeight =
            subf (addf maxWeight (log sums.0)) logParticleCount
          in
          let resampled =
            systematicSample particles expWeights sums.0 particleCount
          in
          let particles = mapReverse (lam p. propagate p contWeight) resampled in
          runRec (particles, addf evidence contWeight)
    in
    let particles = createList particleCount start in
    runRec (particles, 0.)
  in

  -------------
  -- MH PART --
  -------------

  -- Metropolis Hastings iteration
  recursive let mh =
    lam evidences. lam weightss. lam sampless. lam iter.
      if leqi iter 0 then (sampless, weightss, evidences)
      else
        -- Compute a new proposal with a particle filter sweep
        match runSMC () with ((samples, weights), evidence) in
        let prevEvidence = head evidences in
        let prevSamples = head sampless in
        let prevWeights = head weightss in
        let logMhAcceptProb = minf 0. (subf evidence prevEvidence) in
        let iter = subi iter 1 in
        if bernoulliSample (exp logMhAcceptProb) then
          mcmcAccept ();
          mh
            (cons evidence evidences)
            (cons weights weightss)
            (cons samples sampless)
            iter
        else
          mh
            (cons prevEvidence evidences)
            (cons prevWeights weightss)
            (cons prevSamples sampless)
            iter
  in

  -- Initialization
  let runs = config.iterations in

  -- Used to keep track of acceptance ratio
  mcmcAcceptInit runs;

  -- Initial sample
  match runSMC () with ((samples, weights), evidence) in
  let iter = subi runs 1 in

  -- Draw remaining samples
  match mh [evidence] [weights] [samples] iter with (sampless, _, evidences) in

  -- Reverse to get the correct order and flatten
  let samples = join (reverse (sampless)) in
  -- TODO(oerikss, 2022-11-02): what weights should we use
  -- let weights = create runs (lam. 1.) in
  let weights =
    join (map (lam evidence. create particleCount (lam. evidence)) evidences)
  in

  -- Return
  constructDistEmpirical samples weights
    (EmpMCMC { acceptRate = mcmcAcceptRate () })
