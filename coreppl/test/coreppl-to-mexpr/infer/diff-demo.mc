include "../../../models/diff/demo.mc"

mexpr
-- This model is deterministic and the tests consists of assertions in the model
-- code. We simply want to make sure it compiles and runs with all inference
-- algorithms.
infer (Importance { particles = 10 }) model;
infer (BPF { particles = 10 }) model;
infer (APF { particles = 10 }) model;
infer (PIMH { particles = 10, iterations = 10 }) model;
infer (TraceMCMC { iterations = 10 }) model;
infer (NaiveMCMC { iterations = 10 }) model;
infer (LightweightMCMC { iterations = 10, globalProb = 0.1 }) model;
()
