

lang RuntimeDistBase
  syn Dist a =

  sem sample : all a. Dist a -> (() -> a)

  sem logObserve : all a. Dist a -> (a -> Float)

  sem printRes : all a. (a -> String) -> Dist a -> ()
end
