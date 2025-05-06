-- NOTE(vipa, 2025-04-15): The contents of this file must be kept in
-- sync with typeCheckInferMethod and inferMethodConfig of
-- "inference/mcmc-lightweight.mc"

type DebugInfo =
  { accepted : Bool
  }

type Config a acc dAcc =
  { continue : (acc, acc -> a -> (acc, Bool))
  , keepSample : Int -> Bool
  , debug : (dAcc, dAcc -> DebugInfo -> dAcc)
  , globalProb : Float
  , driftKernel : Bool
  }
