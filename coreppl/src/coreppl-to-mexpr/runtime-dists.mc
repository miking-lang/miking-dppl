-- Runtime support for first-class distributions in coreppl-to-mexpr compiler.

include "runtime-dists-base.mc"
include "bpf/dist.mc"
include "importance/dist.mc"

include "math.mc"
include "seq.mc"
include "ext/dist-ext.mc"

lang RuntimeDist = ImportanceRuntimeDist + BPFRuntimeDist
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

  sem sample : all a. Dist a -> a
  sem sample =
  | DistGamma t -> unsafeCoerce (gammaSample t.shape t.scale)
  | DistExponential t -> unsafeCoerce (exponentialSample t.rate)
  | DistPoisson t -> unsafeCoerce (poissonSample t.lambda)
  | DistBinomial t -> unsafeCoerce (binomialSample t.p t.n)
  | DistBernoulli t -> unsafeCoerce (bernoulliSample t.p)
  | DistBeta t -> unsafeCoerce (betaSample t.a t.b)
  | DistGaussian t -> unsafeCoerce (gaussianSample t.mu t.sigma)
  | DistMultinomial t -> unsafeCoerce (multinomialSample t.p t.n)
  | DistCategorical t -> unsafeCoerce (categoricalSample t.p)
  | DistDirichlet t -> unsafeCoerce (dirichletSample t.a)
  | DistUniform t -> unsafeCoerce (uniformContinuousSample t.a t.b)

  sem logObserve : all a. Dist a -> a -> Float
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

  sem printRes : all a. (a -> String) -> Dist a -> String
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

-- We include the below definitions to produce non-mangled functions.
let sample : all a. Dist a -> a =
  use RuntimeDist in
  sample

let logObserve : all a. Dist a -> a -> Float =
  use RuntimeDist in
  logObserve

let printRes : all a. (a -> String) -> Dist a -> () =
  use RuntimeDist in
  printRes
