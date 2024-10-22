mexpr

let m = lam t : (). () in
let d = infer (Importance { particles = 1 }) m in

-- Analytic modifier on the float type
let ok1 = lam x : FloatA. x in

-- PAP modifier on the float type
let ok2 = lam x : FloatP. x in

-- No modifier corresponds to the P (PAP) modifier in the formalization. Which
-- essentially means that the functions with parameters of this type are
-- differentiable in the AD sense.
let ok3 = lam x : Float. x in

-- Non differential modifier on the float type
let ok4 = lam x : FloatN. x in

-- A normal arrow corresponds to a deterministic function (Det modifier in the
-- formalization).
let ok5 = lam f : FloatA -> FloatA. f in

-- The Rnd modifier denotes a random computation.
let ok6 = lam f : FloatA -> Rnd FloatA. f in

-- We need to group it if we have curried functions.
let ok7 = lam f : FloatA -> Rnd (FloatA -> Rnd FloatA). f in

()
