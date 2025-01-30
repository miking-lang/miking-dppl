let choseKernel: all a. use RuntimeDistBase in Dist a -> a -> Float -> Dist a = 
  lam dist. lam prev. lam drift. 
    use RuntimeDistElementary in
  
  switch dist
  case DistBeta _ then
    -- exp = prev, var = prev*(1 - prev)/(drift + 1) --
    DistBeta {a = (mulf drift (unsafeCoerce prev)), b = (mulf drift (subf 1. (unsafeCoerce prev)))}
  case DistBinomial _ then
    -- WARNING : if (drift > prev) => p < 0 --
    -- exp = prev, var = drift --
    let p = divf (subf (unsafeCoerce prev) drift) (unsafeCoerce prev) in
    DistBinomial {n = ceilfi (divf (unsafeCoerce prev) p), p = p}
  case DistBernoulli _ then
    DistBernoulli {p = subf 1.0 (unsafeCoerce prev)}
  case DistCategorical x then
    let driftP = set x.p (unsafeCoerce prev) 0.0 in
    let driftNormalizeP = map (divf (subf 1. (get driftP (unsafeCoerce prev)))) driftP in
    DistCategorical {p = driftNormalizeP}
  case DistDirichlet _ then
    -- exp_i = prev_i/sum(prev),  var_i = prev_i*(sum(prev) - prev_i)/(drift*sum(prev)*(sum(prev)+1)) --
    DistDirichlet {a = (map (mulf drift) (unsafeCoerce prev))}
  case DistExponential _ then
    -- exp = prev, var = 0 --
    DistUniform {a = (divf (unsafeCoerce prev) drift), b = (mulf (unsafeCoerce prev) drift)}
  case DistGamma _ then
    -- exp = prev,  var = prev*drift --
    DistGamma {shape = (divf (unsafeCoerce prev) drift), scale = drift}
  case DistGaussian _ then
    -- exp = prev, var = drift --
    DistGaussian {mu = (unsafeCoerce prev), sigma = drift}
  case DistMultinomial _ then
    -- WARNING : if (prev > drift) => p > 1 AND if drift < 1 => n <1 --
    -- exp_i = prev_i,  var_i = prev_i (1 - prev_i/drift_1)
    let n = ceilfi drift in
    DistMultinomial {n = n, p = map (divf (unsafeCoerce prev)) (unsafeCoerce n)}
  case DistPoisson _ then
    -- WARNING : if (drift > prev) => p < 0 --
    -- exp = prev, var = drift --
    let p = divf (subf (unsafeCoerce prev) drift) (unsafeCoerce prev) in
    DistBinomial {n = ceilfi (divf (unsafeCoerce prev) p), p = p}
  case DistUniform _ then --
    -- exp = prev, var = 0 --
    DistUniform {a = (subf (unsafeCoerce prev) drift), b = (addf (unsafeCoerce prev) drift)}
  case _ then
    dist
  end