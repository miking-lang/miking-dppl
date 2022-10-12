include "../runtime-dists-base.mc"

lang ImportanceRuntimeDist = RuntimeDistBase
  syn Dist a =
  | DistImportance {weights : [Float], samples : [a]}

  sem sample =
  | DistImportance t -> error "Sampling not supported for importance method"

  sem logObserve =
  | DistImportance t -> error "Log observe not supported for importance method"

  sem printRes printFun =
  | DistImportance t ->
    printLn (float2string (normConstant t.weights));
    printSamples printFun t.weights t.samples
end
