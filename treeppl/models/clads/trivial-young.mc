include "ext/math-ext.mc"

-- This file is instead of IO
-- The values should be supplied from "outside" somehow
let g = Globals {
  logActivity = 0.,
  logTurnoverRate = 1.0,
  rateVariance = sqrt (divf 0.2 1.),
  observeProb = 0.5684210526315789
}

let tree = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 1.0}
