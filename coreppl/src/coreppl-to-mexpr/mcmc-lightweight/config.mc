-- NOTE(vipa, 2025-04-15): The contents of this file must be kept in
-- sync with typeCheckInferMethod and inferMethodConfig of
-- "inference/mcmc-lightweight.mc"

type DebugInfo =
  { accepted : Bool
  }

type Config a acc accInit sInfo dAcc =
  { continue : (accInit -> acc, acc -> sInfo -> a -> (acc, Bool))
  , keepSample : Int -> Bool
  , debug : (dAcc, dAcc -> DebugInfo -> dAcc)
  , init : () -> ()
  , temperature : acc -> Float
  , globalProb : Float
  , driftKernel : Bool
  , pigeons : Bool
  , pigeonsGlobal : Bool
  , pigeonsExploreSteps : Int
  }
