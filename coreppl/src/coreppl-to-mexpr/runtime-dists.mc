-- Runtime support for first-class distributions in coreppl-to-mexpr compiler.

include "runtime-dists-base.mc"
include "bpf/dist.mc"
include "importance/dist.mc"

include "math.mc"
include "seq.mc"
include "ext/dist-ext.mc"

lang RuntimeDist = RuntimeDistBase + ImportanceRuntimeDist + BPFRuntimeDist
  syn Dist a =
  | DistGamma {shape : Float, scale : Float}
  | DistExponential {rate : Float}
  | DistPoisson {lambda : Float}
  | DistBinomial {n : Int, p : Float}
  | DistBernoulli {p : Float}
  | DistBeta {a : Float, b : Float}
  | DistGaussian {mu : Float, sigma : Float}
  | DistMultinomial {n : Int, p : [Float]}
  | DistCategorical {p : [Float]}
  | DistDirichlet {a : [Float]}
  | DistUniform {a : Float, b : Float}

  sem sample =
  | DistGamma t -> lam. unsafeCoerce (gammaSample t.shape t.scale)
  | DistExponential t -> lam. unsafeCoerce (exponentialSample t.rate)
  | DistPoisson t -> lam. unsafeCoerce (poissonSample t.lambda)
  | DistBinomial t -> lam. unsafeCoerce (binomialSample t.p t.n)
  | DistBernoulli t -> lam. unsafeCoerce (bernoulliSample t.p)
  | DistBeta t -> lam. unsafeCoerce (betaSample t.a t.b)
  | DistGaussian t -> lam. unsafeCoerce (gaussianSample t.mu t.sigma)
  | DistMultinomial t -> lam. unsafeCoerce (multinomialSample t.p t.n)
  | DistCategorical t -> lam. unsafeCoerce (categoricalSample t.p)
  | DistDirichlet t -> lam. unsafeCoerce (dirichletSample t.a)
  | DistUniform t -> lam. unsafeCoerce (uniformContinuousSample t.a t.b)

  sem logObserve =
  | DistGamma t -> unsafeCoerce (gammaLogPdf t.shape t.scale)
  | DistExponential t -> unsafeCoerce (exponentialLogPdf t.rate)
  | DistPoisson t -> unsafeCoerce (poissonLogPmf t.lambda)
  | DistBinomial t -> unsafeCoerce (binomialLogPmf t.p t.n)
  | DistBernoulli t -> unsafeCoerce (bernoulliLogPmf t.p)
  | DistBeta t -> unsafeCoerce (betaLogPdf t.a t.b)
  | DistGaussian t -> unsafeCoerce (gaussianLogPdf t.mu t.sigma)
  | DistMultinomial t ->
    unsafeCoerce (lam o.
      if eqi t.n (foldl1 addi o) then multinomialLogPmf t.p o
      else negf inf)
  | DistCategorical t -> unsafeCoerce (categoricalLogPmf t.p)
  | DistDirichlet t -> unsafeCoerce (dirichletLogPdf t.a)
  | DistUniform t -> unsafeCoerce (uniformContinuousLogPdf t.a t.b)

  sem printRes printFun =
  | DistGamma _
  | DistExponential _
  | DistPoisson _
  | DistBinomial _
  | DistBernoulli _
  | DistBeta _
  | DistGaussian _
  | DistMultinomial _
  | DistCategorical _
  | DistDirichlet _
  | DistUniform _ -> error "printRes is not implemented for default distributions"
end
