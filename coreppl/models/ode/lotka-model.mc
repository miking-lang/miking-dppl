-- Lotka-Volterra's predator-pray model, where `x.0`, `x.1` are the prey,
-- predator densities, respectivly. The parameters `p = (p0, p1, p2, 03)`,
-- parametrizes:
-- - p0: prey per-capita growth rate
-- - p1: prey death rate from predators
-- - p2: predator per-capita growth rate
-- - p3: predator death rate from lack of prey
let lotkaVolterra = lam p : (Float, Float, Float, Float). lam x : (Float, Float).
  (subf (mulf p.0 x.0) (mulf p.1 (mulf x.0 x.1)),
   subf (mulf p.2 (mulf x.0 x.1)) (mulf p.3 x.1))
