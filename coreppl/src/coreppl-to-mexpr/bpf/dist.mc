include "../runtime-dists-base.mc"

lang BPFRuntimeDist = RuntimeDistBase
  syn Dist a =
  | DistBPF {weights : [Float], samples : [a]}

  sem sample =
  | DistBPF t -> error "Sampling not supported for BPF method"

  sem logObserve =
  | DistBPF t -> error "Log observe not supported for BPF method"

  sem printRes printFun =
  | DistBPF t ->
    printLn (float2string (normConstant t.weights));
    printSamples printFun t.weights t.samples
end
