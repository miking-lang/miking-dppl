-- This is an implementation of functions lifting operators over reals to nested
-- dual-bers described in the paper:

-- Siskind, Jeffrey Mark, and Barak A. Pearl mutter. “Nesting Forward-Mode AD in
-- a Functional Framework.” Higher-Order and Symbolic Computation 21, no. 4
-- (December 1, 2008): 361–76. https://doi.org/10.1007/s10990-008-9037-1.

-- Public functions are prefixed with dualnum. Other functions are internal.



include "dualnum-tree.mc"
include "bool.mc"
include "math.mc"
include "common.mc"

-------------
-- ALIASES --
-------------

let _dnum = dualnumCreateDual
let _ltE = dualLtE
let _isDualNum = dualnumIsDualNum
let _epsilon = dualnumEpsilon
let _primal = dualnumPrimal
let _primalRec = dualnumPrimalRec
let _pertubation = dualnumPertubation
-- let _lift2 = dualnumLift2
-- let _lift1 = dualnumLift1
let _genEpsilon = dualGenEpsilon

let _num0 = Primal 0.
let _num1 = Primal 1.
let _num2 = Primal 2.
let _num3 = Primal 3.
let _num4 = Primal 4.
let _num6 = Primal 6.
let _num8 = Primal 8.
let _num10 = Primal 10.

let _e0 = _genEpsilon ()
let _e1 = _genEpsilon ()
let _e2 = _genEpsilon ()
let _e3 = _genEpsilon ()

let _dnum0 = _dnum _e0
let _dnum1 = _dnum _e1
let _dnum010 = _dnum0 _num1 _num0
let _dnum011 = _dnum0 _num1 _num1
let _dnum012 = _dnum0 _num1 _num2
let _dnum020 = _dnum0 _num2 _num0
let _dnum021 = _dnum0 _num2 _num1
let _dnum022 = _dnum0 _num2 _num2
let _dnum031 = _dnum0 _num3 _num1
let _dnum034 = _dnum0 _num3 _num4
let _dnum036 = _dnum0 _num3 _num6
let _dnum040 = _dnum0 _num4 _num0
let _dnum044 = _dnum0 _num4 _num4
let _dnum048 = _dnum0 _num4 _num8
let _dnum111 = _dnum1 _num1 _num1
let _dnum112 = _dnum1 _num1 _num2
let _dnum122 = _dnum1 _num2 _num2
let _dnum134 = _dnum1 _num3 _num4


----------------------------------
-- LIFTING OF BINARY OPERATORS  --
----------------------------------

type Cmp2 = Float -> Float -> Bool

-- lifts compare function to nested dual-numbers. The compare functions is
-- applied to x in x+(en)x', where x and x' are reals, e.g. x+(en)x' is the
-- deepest element in the nested dual-number y+(e1)y'.
-- cmp : real compare function
let dualnumLiftBoolFun2 : Cmp2 -> (DualNum -> DualNum -> Bool) =
lam cmp. lam p1. lam p2. cmp (_primalRec p1) (_primalRec p2)

let _liftBool = dualnumLiftBoolFun2


----------------
-- CONSTANTS  --
----------------

let epsn = Primal 1.e-15


-----------------------
-- BOOLEAN OPERATORS  --
-----------------------

let eqn = _liftBool eqf -- lifted ==

utest eqn _num1 _num1 with true
utest eqn _num1 _num2 with false
utest eqn (_dnum _e2 _dnum112 _num3) _num1 with true
utest eqn (_dnum _e2 _dnum112 _num3) _num2 with false


let neqn = _liftBool neqf -- lifted !=

utest neqn _num1 _num1 with false
utest neqn _num1 _num2 with true
utest neqn (_dnum _e2 _dnum112 _num3) _num1 with false
utest neqn (_dnum _e2 _dnum112 _num3) _num2 with true


let ltn = _liftBool ltf -- lifted <

utest ltn _num1 _num1 with false
utest ltn _num1 _num2 with true
utest ltn _num2 _num1 with false
utest ltn (_dnum _e2 _dnum112 _num3) _num1 with false
utest ltn (_dnum _e2 _dnum112 _num3) _num2 with true
utest ltn _num2 (_dnum _e2 _dnum112 _num3) with false


let leqn = _liftBool leqf -- lifted <=

utest leqn _num1 _num1 with true
utest leqn _num1 _num2 with true
utest leqn _num2 _num1 with false
utest leqn (_dnum _e2 _dnum112 _num3) _num1 with true
utest leqn (_dnum _e2 _dnum112 _num3) _num2 with true
utest leqn _num2 (_dnum _e2 _dnum112 _num3) with false


let gtn = _liftBool gtf -- lifted >

utest gtn _num1 _num1 with false
utest gtn _num1 _num2 with false
utest gtn _num2 _num1 with true
utest gtn (_dnum _e2 _dnum112 _num3) _num1 with false
utest gtn (_dnum _e2 _dnum112 _num3) _num2 with false
utest gtn _num2 (_dnum _e2 _dnum112 _num3) with true


let geqn = _liftBool geqf -- lifted >=

utest geqn _num1 _num1 with true
utest geqn _num1 _num2 with false
utest geqn _num2 _num1 with true
utest geqn (_dnum _e2 _dnum112 _num3) _num1 with true
utest geqn (_dnum _e2 _dnum112 _num3) _num2 with false
utest geqn _num2 (_dnum _e2 _dnum112 _num3) with true


----------------------------------
-- ADDITION AND MULTIPLICATION  --
----------------------------------

let _eqfApprox = eqfApprox 1.e-6

-- lifted addition
recursive let addn = lam p1. lam p2.
  switch (p1, p2)
  case (Primal p1, Primal p2) then Primal (addf p1 p2)
  case (p & Primal _, Dual r) | (Dual r, p & Primal _) then
    Dual { r with x = addn p r.x }
  case (Dual r1, Dual r2) then
    if _ltE r1.e r2.e then Dual { r2 with x = addn p1 r2.x }
    else if _ltE r2.e r1.e then Dual { r1 with x = addn r1.x p2 }
    else Dual { r1 with x = addn r1.x r2.x, xp = addn r1.xp r2.xp }
  end
end

utest addn _num1 _num2 with _num3 using dualnumEq _eqfApprox
utest addn _dnum010 _num2 with _dnum0 _num3 _num0 using dualnumEq _eqfApprox
utest addn _dnum011 _num2 with _dnum031 using dualnumEq _eqfApprox
utest addn _dnum011 _dnum011 with _dnum022 using dualnumEq _eqfApprox
utest addn _dnum011 _dnum111 with _dnum1 _dnum021 _num1 using dualnumEq _eqfApprox

-- lifted multiplication
recursive let muln = lam p1. lam p2.
  switch (p1, p2)
  case (Primal p1, Primal p2) then Primal (mulf p1 p2)
  case (p & Primal _, Dual r) | (Dual r, p & Primal _) then
    Dual { r with x = muln p r.x, xp = muln p r.xp }
  case (Dual r1, Dual r2) then
    if _ltE r1.e r2.e then
      Dual { r2 with x = muln p1 r2.x, xp = muln p1 r2.xp }
    else if _ltE r2.e r1.e then
      Dual { r1 with x = muln r1.x p2, xp = muln p2 r1.xp }
    else
      Dual {
        r1 with
        x = muln r1.x r2.x,
        xp = addn (muln r2.x r1.xp) (muln r1.x r2.xp)
      }
  end
end

utest muln _num1 _num2 with _num2 using dualnumEq _eqfApprox
utest muln _dnum010 _num2 with _dnum0 _num2 _num0 using dualnumEq _eqfApprox
utest muln _dnum011 _num2 with _dnum022 using dualnumEq _eqfApprox
utest muln _dnum012 _dnum034 with _dnum0 _num3 _num10 using dualnumEq _eqfApprox
utest muln _dnum012 _dnum134 with _dnum1 _dnum036 _dnum048 using dualnumEq _eqfApprox


---------------------------
-- DERIVATIVE OPERATORS  --
---------------------------

-- We define the scalar derivative operator over dual numbers
let der : (DualNum -> DualNum) -> DualNum -> DualNum =
  lam f. lam x.
  let e = _genEpsilon () in
  _pertubation e (f (_dnum e x (Primal 1.)))

utest der (lam. _num2) _num2 with _num0 using dualnumEq eqf
utest der (lam x. muln x x) (_num2) with _num4 using dualnumEq eqf

-- As well as scalar higher order derivatives
recursive
let nder
  : Int -> (DualNum -> DualNum) -> DualNum -> DualNum =
  lam n. lam f.
    if lti n 0 then error "Negative derivative order"
    else if eqi n 0 then f
    else nder (subi n 1) (der f)
end

utest nder 0 (lam x. muln x x) (_num2) with _num4 using dualnumEq eqf
utest nder 1 (lam x. muln x x) (_num4) with _num8 using dualnumEq eqf
utest nder 2 (lam x. muln x x) (_num4) with _num2 using dualnumEq eqf

-- Computes the total derivative
let total : ([DualNum] -> [DualNum]) -> [DualNum] -> ([DualNum] -> [DualNum]) =
  lam f. lam x. lam v.
  let e = _genEpsilon () in
  map (_pertubation e) (f (zipWith (_dnum e) x v))

-------------------------------------
-- REAMINING ARITHMETIC OPERATORS  --
-------------------------------------

-- lifted negation
recursive let negn = lam p.
  switch p
  case Primal p then Primal (negf p)
  case Dual r then Dual { r with x = negn r.x, xp = negn r.xp }
  end
end

utest negn _num1 with Primal (negf 1.) using dualnumEq _eqfApprox
utest negn _num0 with Primal (negf 0.) using dualnumEq _eqfApprox
utest negn _dnum010 with _dnum0 (Primal (negf 1.)) _num0 using dualnumEq _eqfApprox
utest negn _dnum012 with _dnum0 (Primal (negf 1.)) (Primal (negf 2.))
using dualnumEq _eqfApprox

utest der negn _num1 with negn _num1 using dualnumEq _eqfApprox

-- lifted subtraction
recursive let subn = lam p1. lam p2.
  switch (p1, p2)
  case (Primal p1, Primal p2) then Primal (subf p1 p2)
  case (Primal _, Dual r) then
    Dual { r with x = subn p1 r.x, xp = negn r.xp }
  case(Dual r, Primal _) then
    Dual { r with x = subn r.x p2 }
  case (Dual r1, Dual r2) then
    if _ltE r1.e r2.e then
      Dual { r2 with x = subn p1 r2.x }
    else if _ltE r2.e r1.e then
      Dual { r1 with x = subn r1.x p2, xp = negn r1.xp }
    else
      Dual { r1 with x = subn r1.x r2.x, xp = subn r1.xp r2.xp }
  end
end

utest subn _num2 _num1 with _num1 using dualnumEq _eqfApprox
utest subn _dnum020 _num1 with _dnum0 _num1 _num0 using dualnumEq _eqfApprox
utest subn _dnum021 _num1 with _dnum0 _num1 _num1
  using dualnumEq _eqfApprox
utest subn _dnum022 _dnum011 with _dnum011 using dualnumEq _eqfApprox

utest
  let r = subn _dnum122 _dnum011 in
  dualnumPrimal _e1 r
with _dnum0 _num1 (Primal -1.) using dualnumEq _eqfApprox


-- lifted abs
let absn = lam p. if ltn p _num0 then negn p else p

-- lifted approximate compare function
let eqnEps = lam l. lam r.
  ltn (absn (subn l r)) epsn


-- lifted division
recursive let divn = lam p1. lam p2.
  let dfdx1 = lam x2. divn (Primal 1.) x2 in
  let dfdx2 = lam x1. lam x2. divn (negn x1) (muln x2 x2) in
  switch (p1, p2)
  case (Primal p1, Primal p2) then Primal (divf p1 p2)
  case (Primal _, Dual r) then
    Dual { r with x = divn p1 r.x, xp = muln (dfdx2 p1 r.x) r.xp }
  case (Dual r, Primal _) then
    Dual { r with x = divn r.x p2, xp = muln (dfdx1 p2) r.xp }
  case (Dual r1, Dual r2) then
    if _ltE r1.e r2.e then
      Dual { r2 with x = divn p1 r2.x, xp = muln (dfdx2 p1 r2.x) r2.xp }
    else if _ltE r2.e r1.e then
      Dual { r1 with x = divn r1.x p2, xp = muln (dfdx1 p2) r1.xp }
    else
      Dual {
        r1 with
        x = divn r1.x r2.x,
        xp = addn (muln (dfdx1 r2.x) r1.xp) (muln (dfdx2 r1.x r2.x) r2.xp)
      }
  end
end

utest divn _num4 _num2 with _num2 using dualnumEq _eqfApprox
utest divn _dnum040 _num2 with _dnum0 _num2 _num0 using dualnumEq _eqfApprox
utest divn _dnum044 _num2 with _dnum022 using dualnumEq _eqfApprox

utest divn _dnum012 _dnum034
with _dnum0 (Primal (divf 1. 3.)) (Primal (divf 2. 9.)) using dualnumEq _eqfApprox

utest divn _dnum012 _dnum134
with _dnum1 (_dnum0 (Primal (divf 1. 3.))
                    (Primal (divf 2. 3.)))
            (_dnum0 (Primal (divf (negf 4.) 9.))
                    (Primal (divf (negf 8.) 9.)))
using dualnumEq _eqfApprox

----------------
-- CONSTANTS  --
----------------

let pin = Primal pi

---------------------------
-- ELEMENTARY FUNCTIONS  --
---------------------------

-- Trigonometric functions
recursive
  let sinn = lam p.
    switch p
    case Primal p then Primal (sin p)
    case Dual r then Dual { r with x = sinn r.x, xp = muln (cosn r.x) r.xp }
    end
  let cosn = lam p.
    switch p
    case Primal p then Primal (cos p)
    case Dual r then
      Dual { r with x = cosn r.x, xp = negn (muln (sinn r.x) r.xp)}
    end
end

utest sinn (divn pin _num2) with _num1 using eqnEps
utest sinn _num0 with _num0 using eqnEps

utest cosn (divn pin _num2) with _num0 using eqnEps
utest cosn _num0 with _num1 using eqnEps

utest addn (muln (sinn _num1) (sinn _num1)) (muln (cosn _num1) (cosn _num1))
with _num1 using eqnEps

utest der sinn _num1 with cosn _num1 using eqnEps
utest der cosn _num1 with negn (sinn _num1) using eqnEps

-- Exponential function
recursive
  let expn = lam p.
    switch p
    case Primal p then Primal (exp p)
    case Dual r then Dual { r with x = expn r.x, xp = muln (expn r.x) r.xp }
    end
end

utest expn _num0 with _num1 using eqnEps
utest der expn _num1 with expn _num1 using eqnEps

-- Natural logarithm
recursive
  let logn = lam p.
    switch p
    case Primal p then Primal (log p)
    case Dual r then Dual { r with x = logn r.x, xp = divn r.xp r.x }
    end
end

utest logn _num1 with _num0 using eqnEps
utest logn (expn _num3) with _num3 using eqnEps
utest expn (logn _num3) with _num3 using eqnEps
utest der logn _num1 with _num1 using eqnEps


-- Power function
recursive
  let pown = lam p1. lam p2.
  let dfdx1 = lam x1. lam x2. muln x2 (pown x1 (subn x2 (Primal 1.))) in
  let dfdx2 = lam x1. lam x2.
    if eqn x1 (Primal 0.) then
      if gtn x2 (Primal 0.) then Primal 0.
      else Primal nan
    else
      muln (pown x1 x2) (logn x1)
  in
  switch (p1, p2)
  case (Primal p1, Primal p2) then Primal (pow p1 p2)
  case (Primal _, Dual r) then
    Dual { r with x = pown p1 r.x, xp = muln (dfdx2 p1 r.x) r.xp }
  case (Dual r, Primal _) then
    Dual { r with x = pown r.x p2, xp = muln (dfdx1 r.x p2) r.xp }
  case (Dual r1, Dual r2) then
    if _ltE r1.e r2.e then
      Dual { r2 with x = pown p1 r2.x, xp = muln (dfdx2 p1 r2.x) r2.xp }
    else if _ltE r2.e r1.e then
      Dual { r1 with x = pown r1.x p2, xp = muln (dfdx1 r1.x p2) r1.xp }
    else
      Dual {
        r1 with
        x = pown r1.x r2.x,
        xp = addn (muln (dfdx1 r1.x r2.x) r1.xp) (muln (dfdx2 r1.x r2.x) r2.xp)
      }
  end
end

utest pown _num3 _num2 with Primal 9. using eqnEps
utest der (lam x. pown x _num2) _num3 with _num6 using eqnEps
utest der (pown (expn _num1)) _num2 with expn _num2 using eqnEps
utest der (pown _num0) _num2 with _num0 using eqnEps
utest der (pown _num1) _num0 with _num0 using eqnEps
utest der (pown _num1) _num1 with _num0 using eqnEps

-- Square root
recursive
  let sqrtn = lam p.
    switch p
    case Primal p then Primal (sqrt p)
    case Dual r then
      Dual {
        r with
        x = sqrtn r.x,
        xp = divn r.xp (muln (Primal 2.) (sqrtn r.x))
      }
    end
end

utest sqrtn (Primal 9.) with _num3 using eqnEps
utest der sqrtn (Primal 9.) with divn _num1 _num6 using eqnEps

mexpr

-----------------------
-- DERIVATIVE TESTS  --
-----------------------

let f = lam k. lam xs. match xs with [x1, x2] in [x2, negn (muln k x1)] in
let df = lam k. lam xs. lam vs.
  match k with Primal k in
  match xs with [Primal x1, Primal x2] in
  match vs with [Primal v1, Primal v2] in
  [
    Primal v2,
    Primal (subf (negf (mulf k v1)) x1)
  ]
in

let k = Primal 2. in
let xs = [Primal 2., Primal 3.] in
let vs = [Primal 4., Primal 5.] in

utest
  zipWith
    addn
    (total (f k) xs vs)
    (total (lam ks. f (head ks) xs) [k] [Primal 1.])
  with df k xs vs using eqSeq eqnEps
in

()
