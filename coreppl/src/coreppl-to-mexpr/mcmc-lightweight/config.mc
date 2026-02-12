-- NOTE(vipa, 2025-04-15): The contents of this file must be kept in
-- sync with typeCheckInferMethod and inferMethodConfig of
-- "inference/mcmc-lightweight.mc"

type DebugInfo =
  { accepted : Bool
  }

type SampleInfo =
  { weight : Float
  , priorWeight : Float
  }

type Config a acc dAcc =
  { continue : (() -> acc, acc -> SampleInfo -> a -> (acc, Bool))
  , keepSample : Int -> Bool
  , debug : (dAcc, dAcc -> DebugInfo -> dAcc)
  , temperature : acc -> Float
  , driftKernel : Bool
  , resampleBehavior : (acc -> Int -> (acc,([Bool], Int))) -- iter -> alignedTraceLength -> (iter, ([unalignedTraces to redrawn], Align to redraw (-1 none, -2 all)))
  }
