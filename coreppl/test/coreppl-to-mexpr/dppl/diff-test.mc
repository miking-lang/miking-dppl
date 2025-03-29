-- -*- compile-command : "cppl --test --dppl-frontent diff.mc && ./out && rm ./out" -*-

-- include "common.mc"

mexpr

let abs = lam a : Float. if ltf a 0. then negf a else a in
let eqfApprox = lam a : Float. lam b : Float. ltf (abs (subf a b)) 1.e-12 in

--------------------------------------------------------------------------------
-- Test nested differentiation.
--------------------------------------------------------------------------------

let diff1 = lam f : Float -> Float. lam x : Float. diff f x 1. in

let _pow = lam x : Float. lam n : Int.
  foldl mulf 1. (create n (lam i : Int. x))
in

let f = lam x : Float.
  addf (addf (addf (_pow x 4) (_pow x 3)) (_pow x 2)) x
in

utest f 2.  with 30. in
utest diff1 f 2. with 49. in
utest diff1 (diff1 f) 2. with 62. in
utest diff1 (diff1 (diff1 f)) 2. with 54. in
utest diff1 (diff1 (diff1 (diff1 f))) 2. with 24. in
utest diff1 (diff1 (diff1 (diff1 (diff1 f)))) 2. with 0. in

let inner = lam x : Float. lam y : Float. mulf x y in
let outer = lam x : Float. mulf x (diff1 (inner x) 2.) in
utest diff1 outer 1. with 2. in

--------------------------------------------------------------------------------
-- Test differentiation of different vector representations.
--------------------------------------------------------------------------------

let f = lam x : [Float].
  match x with [x1, x2] in
  [addf (mulf x1 x1) (mulf x2 x2), addf (mulf x1 x2) (mulf x2 x1)] in

let df1 = lam x : [Float]. match x with [x1, x2] in [mulf 2. x1, mulf 2. x2] in
let df2 = lam x : [Float]. match x with [x1, x2] in [mulf 2. x2, mulf 2. x1] in

let x = [2., 3.] in
utest f x with [13., 12.] in
utest diff f x [1., 0.] with df1 x in
utest diff f x [0., 1.] with df2 x in

let f = lam x : (Float, Float).
  (addf (mulf x.0 x.0) (mulf x.1 x.1), addf (mulf x.0 x.1) (mulf x.1 x.0)) in

let df1 = lam x : (Float, Float). (mulf 2. x.0, mulf 2. x.1) in
let df2 = lam x : (Float, Float). (mulf 2. x.1, mulf 2. x.0) in

let x = (2., 3.) in
utest f x with (13., 12.) in
utest diff f x (1., 0.) with df1 x in
utest diff f x (0., 1.) with df2 x in

--------------------------------------------------------------------------------
-- Test comparison functions.
--------------------------------------------------------------------------------

-- eqf
utest eqf 1. 1. with true in
utest eqf 1. 0. with false in
-- This example illustrates a drawback with algorithmic differentiation (the
-- differentiated function is really the identity function which has the
-- derivative: lam x.1.).
utest diff1 (lam x : Float. if eqf x 2. then 2. else x) 1. with 1. in
utest diff1 (lam x : Float. if eqf x 2. then 2. else x) 2. with 0. in
utest diff1 (lam x : Float. if eqf 2. x then 2. else x) 1. with 1. in
utest diff1 (lam x : Float. if eqf 2. x then 2. else x) 2. with 0. in

-- neqf
utest neqf 1. 1. with false in
utest neqf 1. 0. with true in
utest diff1 (lam x : Float. if neqf x 2. then 2. else x) 1. with 0. in
utest diff1 (lam x : Float. if neqf x 2. then 2. else x) 2. with 1. in
utest diff1 (lam x : Float. if neqf 2. x then 2. else x) 1. with 0. in
utest diff1 (lam x : Float. if neqf 2. x then 2. else x) 2. with 1. in

-- ltf
utest ltf 1. 1. with false in
utest ltf 1. 0. with false in
utest ltf 0. 1. with true in
utest diff1 (lam x : Float. if ltf x 2. then 2. else x) 1. with 0. in
utest diff1 (lam x : Float. if ltf x 2. then 2. else x) 2. with 1. in
utest diff1 (lam x : Float. if ltf x 2. then 2. else x) 3. with 1. in
utest diff1 (lam x : Float. if ltf 2. x then 2. else x) 1. with 1. in
utest diff1 (lam x : Float. if ltf 2. x then 2. else x) 2. with 1. in
utest diff1 (lam x : Float. if ltf 2. x then 2. else x) 3. with 0. in

-- leqf
utest leqf 1. 1. with true in
utest leqf 1. 0. with false in
utest leqf 0. 1. with true in
utest diff1 (lam x : Float. if leqf x 2. then 2. else x) 1. with 0. in
utest diff1 (lam x : Float. if leqf x 2. then 2. else x) 2. with 0. in
utest diff1 (lam x : Float. if leqf x 2. then 2. else x) 3. with 1. in
utest diff1 (lam x : Float. if leqf 2. x then 2. else x) 1. with 1. in
utest diff1 (lam x : Float. if leqf 2. x then 2. else x) 2. with 0. in
utest diff1 (lam x : Float. if leqf 2. x then 2. else x) 3. with 0. in

-- gtf
utest gtf 1. 1. with false in
utest gtf 1. 0. with true in
utest gtf 0. 1. with false in
utest diff1 (lam x : Float. if gtf x 2. then 2. else x) 1. with 1. in
utest diff1 (lam x : Float. if gtf x 2. then 2. else x) 2. with 1. in
utest diff1 (lam x : Float. if gtf x 2. then 2. else x) 3. with 0. in
utest diff1 (lam x : Float. if gtf 2. x then 2. else x) 1. with 0. in
utest diff1 (lam x : Float. if gtf 2. x then 2. else x) 2. with 1. in
utest diff1 (lam x : Float. if gtf 2. x then 2. else x) 3. with 1. in

-- geqf
utest geqf 1. 1. with true in
utest geqf 1. 0. with true in
utest geqf 0. 1. with false in
utest diff1 (lam x : Float. if geqf x 2. then 2. else x) 1. with 1. in
utest diff1 (lam x : Float. if geqf x 2. then 2. else x) 2. with 0. in
utest diff1 (lam x : Float. if geqf x 2. then 2. else x) 3. with 0. in
utest diff1 (lam x : Float. if geqf 2. x then 2. else x) 1. with 0. in
utest diff1 (lam x : Float. if geqf 2. x then 2. else x) 2. with 0. in
utest diff1 (lam x : Float. if geqf 2. x then 2. else x) 3. with 1. in

--------------------------------------------------------------------------------
-- Test elementary functions.
--------------------------------------------------------------------------------

utest
  utest addf 2. 3. with 5. in
  utest mulf 2. 3. with 6. in
  utest subf 2. 3. with -1. in
  utest divf 6. 3. with 2. in
  utest negf 1. with -1. in
  utest sin 0. with 0. in
  utest cos 0. with 1. in
  utest exp 0. with 1. in
  utest sqrt 1. with 1. in
  utest pow 2. 0. with 1. in
  utest pow 0. 1. with 0. in

  let test = lam x : (Float, Float). lam dx : (Float, Float).
    -- Test binary elementary functions
    utest subf (addf x.0 x.1) x.1 with x.0 in
    utest subf x.0 (addf x.0 x.1) with negf x.1 in
    utest mulf (divf x.0 x.1) x.1 with x.0 using eqfApprox in
    utest mulf x.0 (divf x.1 x.0) with x.1 using eqfApprox in

    let diff1 = lam f : Float -> Float -> Float. lam x : (Float, Float).
      diff (lam a : Float. f a x.1) x.0 1.
    in
    let diff2 = lam f : Float -> Float -> Float. lam x : (Float, Float).
      diff (f x.0) x.1 1.
    in
    let diffTot =
      lam f : Float -> Float -> Float.
        lam x : (Float, Float).
          lam dx : (Float, Float).
            diff (lam x : (Float, Float). f x.0 x.1) x dx
    in
    -- addf
    let f = lam a : Float. lam b : Float. addf a b in
    let df = lam x : (Float, Float). lam dx : (Float, Float). addf dx.0 dx.1 in
    utest diff1 f x with df x (1., 0.) in
    utest diff2 f x with df x (0., 1.) in
    utest diffTot f x dx with df x dx in
    -- mulf
    let f = lam a : Float. lam b : Float. mulf a b in
    let df = lam x : (Float, Float). lam dx : (Float, Float).
      addf (mulf x.1 dx.0) (mulf x.0 dx.1)
    in
    utest diff1 f x with df x (1., 0.) in
    utest diff2 f x with df x (0., 1.) in
    utest diffTot f x dx with df x dx in
    -- subf
    let f = lam a : Float. lam b : Float. subf a b in
    let df = lam x : (Float, Float). lam dx : (Float, Float). subf dx.0 dx.1 in
    utest diff1 f x with df x (1., 0.) in
    utest diff2 f x with df x (0., 1.) in
    utest diffTot f x dx with df x dx in
    -- divf
    let f = lam a : Float. lam b : Float. divf a b in
    let df = lam x : (Float, Float). lam dx : (Float, Float).
      subf (divf dx.0 x.1) (divf (mulf x.0 dx.1) (mulf x.1 x.1))
    in
    utest diff1 f x with df x (1., 0.) using eqfApprox in
    utest diff2 f x with df x (0., 1.) using eqfApprox in
    utest diffTot f x dx with df x dx using eqfApprox in
    -- pow
    let f = lam a : Float. lam b : Float. pow a b in
    let df = lam x : (Float, Float). lam dx : (Float, Float).
      addf
        (mulf (mulf x.1 (pow x.0 (subf x.1 1.))) dx.0)
        (mulf
           (if eqf x.0 0. then
             if gtf x.1 0. then 0. else divf 0. 0.
            else
             mulf (pow x.0 x.1) (log x.0))
           dx.1)
    in
    utest diff1 f x with df x (1., 0.) using eqfApprox in
    -- NOTE(oerikss, 2025-03-01): Make sure to test with something else than
    -- integer exponents.
    (let x = (x.0, addf x.1 0.1) in
     utest diff1 f x with df x (1., 0.) using eqfApprox in ());
    utest diff2 f x with df x (0., 1.) using eqfApprox in
    utest diffTot f x dx with df x dx using eqfApprox in

    -- Test unary elementary functions
    let x = x.0 in
    let dx = dx.0 in
    utest negf (negf x) with x in
    utest exp (log x) with x using eqfApprox in
    utest log (exp x) with x using eqfApprox in
    utest pow (sqrt x) 2. with x using eqfApprox in
    utest sqrt (pow x 2.) with x using eqfApprox in
    -- negf
    let f = lam x : Float. negf x in
    let df = lam x : Float. lam dx : Float. negf dx in
    utest diff f x dx with df x dx in
    -- sin
    let f = lam x : Float. sin x in
    let df = lam x : Float. lam dx : Float. mulf (cos x) dx in
    utest diff f x dx with df x dx in
    -- cos
    let f = lam x : Float. cos x in
    let df = lam x : Float. lam dx : Float. negf (mulf (sin x) dx) in
    utest diff f x dx with df x dx in
    -- exp
    let f = lam x : Float. exp x in
    let df = lam x : Float. lam dx : Float. mulf (exp x) dx in
    utest diff f x dx with df x dx in
    -- log
    let f = lam x : Float. log x in
    let df = lam x : Float. lam dx : Float. divf dx x in
    utest diff f x dx with df x dx in
    -- sqrt
    let f = lam x : Float. sqrt x in
    let df = lam x : Float. lam dx : Float. divf dx (mulf 2. (sqrt x)) in
    utest diff f x dx with df x dx in
    ()
  in

  let rf = lam t : (). int2float (randIntU 1 100) in
  iter
    (lam t : (). test (rf (), rf ()) (rf (), rf ()))
    (create 100 (lam i : Int. ()));

  () with () in

--------------------------------------------------------------------------------
-- Terms failing float assertions (when float assertions enabled)
--------------------------------------------------------------------------------

let diff1 = lam f : Float -> Float. lam x : Float. diff f x 1. in

-- utest diff1 (lam x. int2float (floorfi x)) 1. with 0. in
-- utest diff1 (lam x. int2float (ceilfi x)) 1. with 0. in
-- utest diff1 (lam x. int2float (roundfi x)) 1. with 0. in

--------------------------------------------------------------------------------
-- Terms not-failing float assertions (when float assertions enabled)
--------------------------------------------------------------------------------

let diff1 = lam f : Float -> Float. lam x : Float. diff f x 1. in

utest diff1 (lam x : Float. mulf (int2float (floorfi 1.)) x) 1. with 1. in
utest diff1 (lam x : Float. mulf (int2float (ceilfi 1.)) x) 1. with 1. in
utest diff1 (lam x : Float. mulf (int2float (roundfi 1.)) x) 1. with 1. in

()
