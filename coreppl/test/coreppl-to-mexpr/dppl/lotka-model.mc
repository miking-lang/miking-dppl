-- Lotka-Volterra's predator-pray model, where `y.0`, `y.1` are the prey,
-- predator densities, respectivly. The parameters `p = (p0, p1, p2, 03)`,
-- parametrizes:
-- - p0: prey per-capita growth rate
-- - p1: prey death rate from predators
-- - p2: predator per-capita growth rate
-- - p3: predator death rate from lack of prey
let lotkaVolterra =
  lam p : (FloatA, FloatA, FloatA, FloatA).
    lam y : (FloatA, FloatA).
      (subf (mulf p.0 y.0) (mulf p.1 (mulf y.0 y.1)),
       subf (mulf p.2 (mulf y.0 y.1)) (mulf p.3 y.1))

mexpr
let m = lam t : (). () in
let d = infer (Importance { particles = 1 }) m in
()
