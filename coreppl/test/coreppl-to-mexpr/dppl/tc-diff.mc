mexpr

let ok1 =
  lam f : FloatA -> FloatA. lam x : FloatA. lam dx : FloatA. diffA f x dx
in

let ok2 =
  lam f : [FloatA] -> [FloatA]. lam x : [FloatA]. lam dx : [FloatA].
    diffA f x dx
in

let ok3 =
  lam f : (FloatA, FloatA) -> (FloatA, FloatP, FloatN).
    lam x : (FloatA, FloatN).
      lam dx : (FloatA, FloatA).
        diffA f x dx
in

let ok4 =
  lam f : FloatP -> FloatA. lam x : FloatP. lam dx : FloatA. diffP f x dx
in

let ok5 =
  lam f : [FloatP] -> [FloatA]. lam x : [FloatP]. lam dx : [FloatA].
    diffP f x dx
in

let ok6 =
  lam f : (FloatP, FloatP) -> (FloatA, FloatP, FloatN).
    lam x : (FloatP, FloatN).
      lam dx : (FloatA, FloatA).
        diffP f x dx
in

-- TYPE ERROR: Trying to differentiate a PAP function with `diffA`.
-- let err1  = lam f : FloatP -> FloatA. lam x : FloatN. lam dx : FloatA.
--   diffA f x dx
-- in

-- TYPE ERROR: Trying to differentiate a non-differentiable function.
-- let err2 = lam f : FloatN -> FloatA. lam x : FloatN. lam dx : FloatA.
--   diffP f x dx
-- in

-- TYPE ERROR: Trying to differentiate a non-deterministic function.
-- let err3 = lam f : FloatA -> Rnd FloatA. lam x : FloatN. lam dx : FloatA.
--   diff f x dx
-- in

-- TYPE ERROR: Different coeffect modifiers.
-- let err4 =
--   lam f : (FloatA, FloatP) -> (FloatA, FloatP, FloatN).
--     lam x : (FloatA, FloatN).
--       lam dx : (FloatA, FloatA).
--         diffP f x dx
-- in

()
