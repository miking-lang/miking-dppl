let add = lam a : (FloatA, FloatA). lam b : (FloatA, FloatA).
  (addf a.0 b.0, addf a.1 b.1)

let smul = lam s : FloatA. lam a : (FloatA, FloatA).
  (mulf s a.0, mulf s a.1)

mexpr

let m = lam t : (). () in
let d = infer (Importance { particles = 1 }) m in

-- ┌────────────────────┐
-- │ The time parameter │
-- └────────────────────┘

-- TYPE ERROR: We dont allow x to have the type FloatA beacuse the solver
-- approximates the solution y(x), hence it is not analytic.
-- let err1 =
--   lam f : FloatA -> FloatA -> FloatA. lam y0 : FloatA. lam x : FloatA.
--     solveode (Default {}) f y0 x
-- in

let ok1 =
  lam f : FloatA -> FloatA -> FloatA. lam y0 : FloatA. lam x : FloatP.
    solveode (Default {}) f y0 x
in

let ok2 =
  lam f : FloatA -> FloatA -> FloatA. lam y0 : FloatA. lam x : FloatN.
    solveode (Default {}) f y0 x
in

-- ┌─────────────────────────────┐
-- │ The initial value parameter │
-- └─────────────────────────────┘

-- TYPE ERROR
-- let err2 =
--   lam f : FloatA -> FloatP -> FloatP. lam y0 : FloatA. lam x : FloatP.
--     solveode (Default {}) f y0 x
-- in

let ok3 =
  lam f : FloatA -> FloatP -> FloatP. lam y0 : FloatP. lam x : FloatP.
    solveode (Default {}) f y0 x
in

let ok4 =
  lam f : FloatA -> FloatP -> FloatP. lam y0 : FloatN. lam x : FloatP.
    solveode (Default {}) f y0 x
in

-- ┌───────────────────────────────────────┐
-- │ The model and initial value parameter │
-- └───────────────────────────────────────┘

let ok5 =
  lam f : FloatA -> FloatP -> FloatP. lam y0 : FloatP. lam x : FloatP.
    solveode (Default {}) f y0 x
in

let ok6 =
  lam f : FloatA -> FloatA -> FloatP. lam y0 : FloatP. lam x : FloatP.
    solveode (Default {}) f y0 x
in

-- TYPE ERROR: This would otherwise allow an unsafe coerce term
-- `id = lam z : FloatA. solve (lam FloatA. lam : FloatA. 0.) z 0.` where
--  `id t` coerces any term `t : FloatA` to `FloatN`.
-- let err3 =
--   lam f : FloatA -> FloatP -> FloatN. lam y0 : FloatP. lam x : FloatP.
--     solveode (Default {}) f y0 x
-- in

-- TYPE ERRORS: Non-deterministic functions
-- let err4 =
--   lam f : FloatA -> FloatA -> Rnd FloatP. lam y0 : FloatP. lam x : FloatP.
--     solveode (Default {}) f y0 x
-- in
-- let err5 =
--   lam f : FloatA -> Rnd (FloatA -> FloatP). lam y0 : FloatP. lam x : FloatP.
--     solveode (Default {}) f y0 x
-- in

-- ┌───────────────┐
-- │ Vector states │
-- └───────────────┘

let ok7 =
  lam f : FloatA -> (FloatA, FloatA) -> (FloatA, FloatA).
    lam x : FloatN.
      lam g : (FloatA, FloatA) -> FloatA.
        lam h : FloatN -> FloatN.
          h (g ((solveode (Default { add = add, smul = smul }) f (0., 0.) x)))
in

-- TYPE ERROR
-- let err6 =
--   lam f : FloatA -> (FloatA, FloatA) -> (FloatA, FloatA).
--     lam x : FloatP.
--       lam g : (FloatA, FloatA) -> FloatA.
--         lam h : FloatN -> FloatN.
--           h (g ((solveode (Default { add = add, smul = smul }) f (0., 0.) x)))
-- in

()
