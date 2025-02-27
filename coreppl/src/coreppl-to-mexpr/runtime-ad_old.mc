-- -*- compile-command : "cppl runtime-ad.mc && ./out && rm ./out" -*-

/-

 This file implements operator lifting to, possibly tagged and nested, dual
 numbers, as well as the runtime implementation of diff.

 The representation of nested dual numbers is based on:
 Siskind, Jeffrey Mark, and Barak A. Pearl mutter. “Nesting Forward-Mode AD in a
 Functional Framework.” Higher-Order and Symbolic Computation 21, no. 4
 (December 1, 2008): 361–76. https://doi.org/10.1007/s10990-008-9037-1.

 The actual lifting of operators is handled by repr types, described in:
 V. Palmkvist, A. Å. Thuné, E. Castegren, and D. Broman, “Repr Types: One
 Abstraction to Rule Them All,” Sep. 12, 2024, arXiv: arXiv:2409.07950. doi:
 10.48550/arXiv.2409.07950.

-/

include "string.mc"
include "seq.mc"
include "math-ext.mc"

-- =============================================================================
-- Representation and operations
-- =============================================================================

type Real = Repr Float

--------------------------------------------------------------------------------
-- Conversion between Float and Real
--------------------------------------------------------------------------------

let fromFloat : Float -> Real = op
let toFloatDiscardTangent : Real -> Float = op
let toFloatAssertNoTangent : Real -> Float = op

let assertNoTangentErrMsg = "Assumed a dual number without a tangent when tangent was present. This error likely stems from trying to differentiate a non-differentiable function."

--------------------------------------------------------------------------------
-- Elementary and comparison functions
--------------------------------------------------------------------------------

let add_r : Real -> Real -> Real = op
let sub_r : Real -> Real -> Real = op
let mul_r : Real -> Real -> Real = op
let div_r : Real -> Real -> Real = op
-- let sin : Real -> Real = op
-- let cos : Real -> Real = op
-- let sqrt : Real -> Real = op
let exp_r : Real -> Real = op
let log_r : Real -> Real = op
-- let pow : Real -> Real -> Real = op
let eq_r : Real -> Real -> Bool = op
let neq_r : Real -> Real -> Bool = op
let lt_r : Real -> Real -> Bool = op
let leq_r : Real -> Real -> Bool = op
let gt_r : Real -> Real -> Bool = op
let geq_r : Real -> Real -> Bool = op

--------------------------------------------------------------------------------
-- Differentiation
--------------------------------------------------------------------------------

let diff : ([Real] -> [Real]) -> [Real] -> [Real] -> [Real] = op
let diff1 : (Real -> Real) -> Real -> Real = op

--------------------------------------------------------------------------------
-- Operations over distributions
--------------------------------------------------------------------------------

let fromDistFloat : Dist Float -> Dist Real = op
let toDistFloat : Dist Real -> Dist Float = op
let assumer : all a. Dist a -> a = op

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

let real2string : Real -> String = op

-- =============================================================================
-- Float implementations
-- =============================================================================

let float : Float -> Float = repr

--------------------------------------------------------------------------------
-- Conversion between Float and Real
--------------------------------------------------------------------------------

let fromFloat
  : _ -> Subst float _
  = impl 0.0 (lam x. x)

let toFloatDiscardTangent
  : Subst float _ -> _
  = impl 0.0 (lam x. x)

let toFloatAssertNoTangent
  : Subst float _ -> _
  = impl 0.0 (lam x. x)

--------------------------------------------------------------------------------
-- Elementary and comparison functions
--------------------------------------------------------------------------------

let add_r
  : Subst float _ -> Subst float _ -> Subst float _
  = impl 1.0 addf

let sub_r
  : Subst float _ -> Subst float _ -> Subst float _
  = impl 1.0 subf

let mul_r
  : Subst float _ -> Subst float _ -> Subst float _
  = impl 1.0 mulf

let div_r
  : Subst float _ -> Subst float _ -> Subst float _
  = impl 1.0 divf

let exp_r
  : Subst float _ -> Subst float _
  = impl 1.0 exp

let log_r
  : Subst float _ -> Subst float _
  = impl 1.0 log

let eq_r
  : Subst float _ -> Subst float _ ->  _
  = impl 1.0 eqf

let neq_r
  : Subst float _ -> Subst float _ ->  _
  = impl 1.0 neqf

let lt_r
  : Subst float _ -> Subst float _ ->  _
  = impl 1.0 ltf

let leq_r
  : Subst float _ -> Subst float _ ->  _
  = impl 1.0 leqf

let gt_r
  : Subst float _ -> Subst float _ ->  _
  = impl 1.0 gtf

let geq_r
  : Subst float _ -> Subst float _ ->  _
  = impl 1.0 geqf

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

let real2string
  : Subst float _ -> _
  = impl 1.0 float2string

--------------------------------------------------------------------------------
-- Operations over distributions
--------------------------------------------------------------------------------

let fromDistFloat
  : _ -> Dist (Subst float _)
  = impl 0.0 (lam x. x)

let toDistFloat
  : Dist (Subst float _) -> _
  = impl 0.0 (lam x. x)

let assumer
  : Dist _ -> _
  = impl 0.0 (lam d. assume d)

let assumer
  : Dist (Subst float _) -> Subst float _
  = impl 0.0 (lam d. assume d)

/-
-- =============================================================================
-- Pair implementations
-- =============================================================================

let pair : Float -> (Float, Float) = repr

let implicitLiftPair : Real -> (Float, Float) = op

let implicitLiftPair
  : Subst float _ -> _
  = impl 1.0 (lam x. (x, 0.0))

let implicitLiftPair
  : Subst pair _ -> _
  = impl 0.0 (lam x. x)

--------------------------------------------------------------------------------
-- Conversion between Float and Real
--------------------------------------------------------------------------------

let fromFloat
  : _ -> Subst pair _
  = impl 1.0 (lam x. (x, 0.0))

let toFloatDiscardTangent
  : Subst pair _ -> _
  = impl 1.0 (lam x. x.0)

let toFloatAssertNoTangent
  : Subst pair _ -> _
  = impl 1.0 (lam. error assertNoTangentErrMsg)

--------------------------------------------------------------------------------
-- Elementary and comparison functions
--------------------------------------------------------------------------------

let add_r
  : _ -> _ -> Subst pair _
  = impl 3.0
    (lam l. lam r.
      let l = implicitLiftPair l in
      let r = implicitLiftPair r in
      (addf l.0 r.0, addf l.1 r.1))

let sub_r
  : _ -> _ -> Subst pair _
  = impl 3.0
    (lam l. lam r.
      let l = implicitLiftPair l in
      let r = implicitLiftPair r in
      (subf l.0 r.0, subf l.1 r.1))

let mul_r
  : _ -> _ -> Subst pair _
  = impl 4.0
    (lam l. lam r.
      let l = implicitLiftPair l in
      let r = implicitLiftPair r in
      (mulf l.0 r.0, addf (mulf l.0 r.1) (mulf l.1 r.0)))

let div_r
  : _ -> _ -> Subst pair _
  = impl 6.0
    (lam l. lam r.
      let l = implicitLiftPair l in
      let r = implicitLiftPair r in
      (divf l.0 r.0, divf (subf (mulf l.1 r.0) (mulf l.0 r.1)) (mulf r.0 r.0)))

--------------------------------------------------------------------------------
-- Differentiation
--------------------------------------------------------------------------------

let diff
  : ([Subst pair _] -> [Subst pair _]) -> [Subst float _] -> [Subst float _]
  -> [Subst float _]
  = impl 10.0                   -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam f. lam x. lam dx.
      (unzip (f (zip x dx))).1)

let diff1
  : (Subst pair _ -> Subst pair _) -> Subst float _ -> Subst float _
  = impl 2.0
    (lam f. lam x.
      (f (x, 1.0)).1)

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

let real2string
  : Subst pair _ -> _
  = impl 6.0 (lam x. join ["(", float2string x.0, ", ", float2string x.1, ")"])
-/
-- =============================================================================
-- Tagged dual number tree implementations
-- =============================================================================

type Eps = Int

-- Dual's can be nested and are implemented as explicit trees.
type Dual a
con Real : all a. a -> Dual a
con Dual : all a. {e : Eps, x : Dual a, xp : Dual a} -> Dual a

-- Epsilons are ordered.
let epsLt : Eps -> Eps -> Bool = lti

-- x in x+e1(x+e2(x+e3(...)))
recursive
let primalRec : all a. Dual a -> a =
lam n.
  switch n
  case Real n then n
  case Dual {x = x} then primalRec x
  end
end

-- Unboxes x where x is assumed to have no tangent.
let unboxReal : all a. Dual a -> a =
lam n.
  switch n
  case Real n then n
  case _ then error assertNoTangentErrMsg
  end

-- x' in x+ex' given e
let tangent : all a. a -> Eps -> Dual a -> Dual a =
lam zero. lam e. lam n.
  switch n
  case Real _ then Real zero
  case Dual dn then if lti dn.e e then Real zero else dn.xp
  end

-- generate a unique epsilon e1 that fulfills the invariant e1 > e for all
-- previously generated epsilons e.
let e = ref 0
let geneps : () -> Eps =
lam. modref e (subi (deref e) 1); deref e

let dual : Float -> Dual Float = repr

let implicitLiftDual : Real -> Dual Float = op

let implicitLiftDual
  : Subst float _ -> _
  = impl 1.0 (lam x. Real x)

--------------------------------------------------------------------------------
-- Conversion between Float and Real
--------------------------------------------------------------------------------

let toFloatDiscardTangent
  : Subst dual _ -> _
  = impl 3.0 primalRec          -- TODO(oerikss, 2025-02-11): Tweak cost

let toFloatAssertNoTangent
  : Subst dual _ -> _
  = impl 3.0 unboxReal          -- TODO(oerikss, 2025-02-11): Tweak cost

--------------------------------------------------------------------------------
-- Elementary and comparison functions
--------------------------------------------------------------------------------

recursive let addn = lam p1. lam p2.
  switch (p1, p2)
  case (Real p1, Real p2) then Real (addf p1 p2)
  case (p & Real _, Dual r) | (Dual r, p & Real _) then
    Dual { r with x = addn p r.x }
  case (Dual r1, Dual r2) then
    if epsLt r1.e r2.e then Dual { r2 with x = addn p1 r2.x }
    else if epsLt r2.e r1.e then Dual { r1 with x = addn r1.x p2 }
    else Dual { r1 with x = addn r1.x r2.x, xp = addn r1.xp r2.xp }
  end
end

recursive let negn = lam p.
  switch p
  case Real p then Real (negf p)
  case Dual r then Dual { r with x = negn r.x, xp = negn r.xp }
  end
end

recursive let subn = lam p1. lam p2.
  switch (p1, p2)
  case (Real p1, Real p2) then Real (subf p1 p2)
  case (Real _, Dual r) then
    Dual { r with x = subn p1 r.x, xp = negn r.xp }
  case(Dual r, Real _) then
    Dual { r with x = subn r.x p2 }
  case (Dual r1, Dual r2) then
    if epsLt r1.e r2.e then
      Dual { r2 with x = subn p1 r2.x }
    else if epsLt r2.e r1.e then
      Dual { r1 with x = subn r1.x p2, xp = negn r1.xp }
    else
      Dual { r1 with x = subn r1.x r2.x, xp = subn r1.xp r2.xp }
  end
end

recursive let muln = lam p1. lam p2.
  switch (p1, p2)
  case (Real p1, Real p2) then Real (mulf p1 p2)
  case (p & Real _, Dual r) | (Dual r, p & Real _) then
    Dual { r with x = muln p r.x, xp = muln p r.xp }
  case (Dual r1, Dual r2) then
    if epsLt r1.e r2.e then
      Dual { r2 with x = muln p1 r2.x, xp = muln p1 r2.xp }
    else if epsLt r2.e r1.e then
      Dual { r1 with x = muln r1.x p2, xp = muln p2 r1.xp }
    else
      Dual {
        r1 with
        x = muln r1.x r2.x,
        xp = addn (muln r2.x r1.xp) (muln r1.x r2.xp)
      }
  end
end

recursive let divn = lam p1. lam p2.
  let dfdx1 = lam x2. divn (Real 1.) x2 in
  let dfdx2 = lam x1. lam x2. divn (negn x1) (muln x2 x2) in
  switch (p1, p2)
  case (Real p1, Real p2) then Real (divf p1 p2)
  case (Real _, Dual r) then
    Dual { r with x = divn p1 r.x, xp = muln (dfdx2 p1 r.x) r.xp }
  case (Dual r, Real _) then
    Dual { r with x = divn r.x p2, xp = muln (dfdx1 p2) r.xp }
  case (Dual r1, Dual r2) then
    if epsLt r1.e r2.e then
      Dual { r2 with x = divn p1 r2.x, xp = muln (dfdx2 p1 r2.x) r2.xp }
    else if epsLt r2.e r1.e then
      Dual { r1 with x = divn r1.x p2, xp = muln (dfdx1 p2) r1.xp }
    else
      Dual {
        r1 with
        x = divn r1.x r2.x,
        xp = addn (muln (dfdx1 r2.x) r1.xp) (muln (dfdx2 r1.x r2.x) r2.xp)
      }
  end
end

recursive let expn = lam p.
  switch p
  case Real p then Real (exp p)
  case Dual r then Dual { r with x = expn r.x, xp = muln (expn r.x) r.xp }
  end
end

recursive
  let logn = lam p.
    switch p
    case Real p then Real (log p)
    case Dual r then Dual { r with x = logn r.x, xp = divn r.xp r.x }
    end
end

let add_r
  : Subst dual _ -> Subst dual _ -> Subst dual _
  = impl 6.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    addn

let add_r
  : Subst dual _ -> Subst float _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. addn l (Real r))

let add_r
  : Subst float _ -> Subst dual _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. addn (Real l) r)

let sub_r
  : Subst dual _ -> Subst dual _ -> Subst dual _
  = impl 6.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    subn

let sub_r
  : Subst dual _ -> Subst float _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. subn l (Real r))

let sub_r
  : Subst float _ -> Subst dual _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. subn (Real l) r)

let mul_r
  : Subst dual _ -> Subst dual _ -> Subst dual _
  = impl 8.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    muln

let mul_r
  : Subst dual _ -> Subst float _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. muln l (Real r))

let mul_r
  : Subst float _ -> Subst dual _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. muln (Real l) r)

let div_r
  : Subst dual _ -> Subst dual _ -> Subst dual _
  = impl 12.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    divn

let div_r
  : Subst dual _ -> Subst float _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. divn l (Real r))

let div_r
  : Subst float _ -> Subst dual _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r. divn (Real l) r)

let exp_r
  : Subst dual _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    expn

let exp_r
  : Subst float _ -> Subst dual _
  = impl 2.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam x. expn (Real x))

let log_r
  : Subst dual _ -> Subst dual _
  = impl 3.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    logn

let log_r
  : Subst float _ -> Subst dual _
  = impl 2.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam x. logn (Real x))

let eq_r
  : _ ->  _ ->  _
  = impl 1.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam l. lam r.
      eqf (toFloatDiscardTangent l) (toFloatDiscardTangent r))


-- let neqr
--   : Subst float _ -> Subst float _ ->  _
--   = impl 1.0 neqf

-- let ltr
--   : Subst float _ -> Subst float _ ->  _
--   = impl 1.0 ltf

-- let leqr
--   : Subst float _ -> Subst float _ ->  _
--   = impl 1.0 leqf

-- let gtr
--   : Subst float _ -> Subst float _ ->  _
--   = impl 1.0 gtf

-- let geqr
--   : Subst float _ -> Subst float _ ->  _
--   = impl 1.0 geqf


--------------------------------------------------------------------------------
-- Differentiation
--------------------------------------------------------------------------------

let diff_ = lam f. lam xs. lam xps.
  let e = geneps () in
  let xs = zipWith (lam x. lam xp. Dual { e = e, x = x, xp = xp }) xs xps in
  map (tangent 0. e) (f xs)

let diff
  : ([Subst dual _] -> [Subst dual _]) -> [Subst float _] -> [Subst float _] -> [Subst dual _]
  = impl 16.0                   -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam f. lam xs. lam xps.
       diff_ f (map (lam x. Real x) xs) (map (lam xp. Real xp) xps))

let diff
  : ([Subst dual _] -> [Subst dual _]) -> [Subst dual _] -> [Subst float _] -> [Subst dual _]
  = impl 14.0                   -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam f. lam xs. lam xps.
       diff_ f xs (map (lam xp. Real xp) xps))

let diff
  : ([Subst dual _] -> [Subst dual _]) -> [Subst float _] -> [Subst dual _] -> [Subst dual _]
  = impl 14.0                   -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam f. lam xs. lam xps.
       diff_ f (map (lam x. Real x) xs) xps)

let diff
  : ([Subst dual _] -> [Subst dual _]) -> [Subst dual _] -> [Subst dual _] -> [Subst dual _]
  = impl 12.0                   -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam f. lam xs. lam xps.
       let e = geneps () in
       map (tangent 0. e)
         (f (zipWith (lam x. lam xp. Dual { e = e, x = x, xp = xp }) xs xps)))

let diff1_ = lam f. lam x. lam xp.
  let e = geneps () in tangent 0. e (f (Dual { e = e, x = x, xp = xp }))

let diff1
  : (Subst dual _ -> Subst dual _) -> Subst float _ -> Subst dual _
  = impl 4.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam f. lam x. diff1_ f (Real x) (Real 1.))

let diff1
  : (Subst dual _ -> Subst dual _) -> Subst dual _ -> Subst dual _
  = impl 4.0                    -- TODO(oerikss, 2025-02-11): Tweak cost.
    (lam f. lam x. diff1_ f x (Real 1.))

--------------------------------------------------------------------------------
-- Operations over distributions
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

recursive let dual2string = lam n.
  switch n
  case Real x then float2string x
  case Dual r then
    join ["(", dual2string r.x, " + ", int2string r.e, dual2string r.xp]
  end
end

let real2string
  : Subst dual _ -> _
  = impl 12.0 dual2string

mexpr

utest toFloatDiscardTangent (fromFloat 42.0) with 42. in

--------------------------------------------------------------------------------
-- Test diff1
--------------------------------------------------------------------------------

let f = lam x. add_r (add_r (mul_r x x) x) (fromFloat 1.0) in
let df = lam x. addf (mulf 2. x) 1. in

let test = lam x. toFloatDiscardTangent (diff1 f (fromFloat x)) in
let x = 1. in utest test x with df x in
let x = 2. in utest test x with df x in
let x = 3. in utest test x with df x in
let x = 4. in utest test x with df x in

let f = lam x. mul_r x (diff1 (lam y. mul_r x y) (fromFloat 2.)) in
let df = lam x. mulf 2. x in

let test = lam x. toFloatDiscardTangent (diff1 f (fromFloat x)) in
let x = 1. in utest test x with df x in

--------------------------------------------------------------------------------
-- Test diff
--------------------------------------------------------------------------------

let f = lam x.
  match x with [x1, x2] in
  [add_r (add_r (mul_r x1 x2) x1) (fromFloat 1.)
  ,add_r (add_r (mul_r x1 x2) (mul_r (fromFloat 2.) x2)) (fromFloat 2.)
  ]
in
let df0 = lam x. match x with [x1, x2] in [addf x2 1., x2] in
let df1 = lam x. match x with [x1, x2] in [x1, addf x1 2.] in

let test0 = lam x.
  map toFloatDiscardTangent
    (diff f (map fromFloat x) [fromFloat 1., fromFloat 0.])
in
let x = [1., 2.] in utest test0 x with df0 x in
let x = [2., 3.] in utest test0 x with df0 x in
let x = [3., 4.] in utest test0 x with df0 x in
let x = [4., 5.] in utest test0 x with df0 x in

let test1 = lam x.
  map toFloatDiscardTangent
    (diff f (map fromFloat x) [fromFloat 0., fromFloat 1.])
in
let x = [1., 2.] in utest test1 x with df1 x in
let x = [2., 3.] in utest test1 x with df1 x in
let x = [3., 4.] in utest test1 x with df1 x in
let x = [4., 5.] in utest test1 x with df1 x in

--------------------------------------------------------------------------------
-- Test Lifted PPL constructs
--------------------------------------------------------------------------------

let m = lam.
  assumer
    (fromDistFloat
       (Gaussian
         (toFloatAssertNoTangent (fromFloat 1.))
       (toFloatAssertNoTangent (fromFloat 1.))))
in
let d = infer (Importance { particles = 1 }) m in

let m = lam.
  assumer
    (Poisson
      (toFloatAssertNoTangent (fromFloat 1.)))
in
let d = infer (Importance { particles = 1 }) m in

let m = lam.
  let c = assumer
    (fromDistFloat
       (Gaussian
         (toFloatAssertNoTangent (fromFloat 1.))
       (toFloatAssertNoTangent (fromFloat 1.))))
  in
  diff1 (lam x. mul_r c x) c
in
let d = infer (Importance { particles = 1 }) m in

()
