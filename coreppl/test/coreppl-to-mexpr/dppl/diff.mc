-- -*- compile-command : "cppl --test test.mc && ./out && rm ./out" -*-

include "common.mc"

mexpr

let abs = lam a. if ltf a 0. then negf a else a in
let eqfApprox = lam a. lam b. ltf (abs (subf a b)) 1.e-12 in

--------------------------------------------------------------------------------
-- Test nested differentiation.
--------------------------------------------------------------------------------

let diff1 = lam f : Float -> Float. lam x : Float. diff f x 1. in

recursive let _pow = lam x. lam n.
  if lti n 0 then error "Invalid input"
  else
    if eqi n 0 then 1.
    else mulf x (_pow x (subi n 1))
in

let f = lam x.
  addf (addf (addf (_pow x 4) (_pow x 3)) (_pow x 2)) x
in

utest f 2.  with 30. in
utest diff1 f 2. with 49. in
utest diff1 (diff1 f) 2. with 62. in
utest diff1 (diff1 (diff1 f)) 2. with 54. in
utest diff1 (diff1 (diff1 (diff1 f))) 2. with 24. in
utest diff1 (diff1 (diff1 (diff1 (diff1 f)))) 2. with 0. in

let inner = lam x. lam y. mulf x y in
let outer = lam x. mulf x (diff1 (inner x) 2.) in
utest diff1 outer 1. with 2. in

--------------------------------------------------------------------------------
-- Test differentiation of different vector representations.
--------------------------------------------------------------------------------

let f = lam x.
  match x with [x1, x2] in
  [addf (mulf x1 x1) (mulf x2 x2), addf (mulf x1 x2) (mulf x2 x1)] in

let df1 = lam x. match x with [x1, x2] in [mulf 2. x1, mulf 2. x2] in
let df2 = lam x. match x with [x1, x2] in [mulf 2. x2, mulf 2. x1] in

let x = [2., 3.] in
utest f x with [13., 12.] in
utest diff f x [1., 0.] with df1 x in
utest diff f x [0., 1.] with df2 x in

let f = lam x.
  (addf (mulf x.0 x.0) (mulf x.1 x.1), addf (mulf x.0 x.1) (mulf x.1 x.0)) in

let df1 = lam x. (mulf 2. x.0, mulf 2. x.1) in
let df2 = lam x. (mulf 2. x.1, mulf 2. x.0) in

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
utest diff1 (lam x. if eqf x 2. then 2. else x) 1. with 1. in
utest diff1 (lam x. if eqf x 2. then 2. else x) 2. with 0. in
utest diff1 (lam x. if eqf 2. x then 2. else x) 1. with 1. in
utest diff1 (lam x. if eqf 2. x then 2. else x) 2. with 0. in

-- neqf
utest neqf 1. 1. with false in
utest neqf 1. 0. with true in
utest diff1 (lam x. if neqf x 2. then 2. else x) 1. with 0. in
utest diff1 (lam x. if neqf x 2. then 2. else x) 2. with 1. in
utest diff1 (lam x. if neqf 2. x then 2. else x) 1. with 0. in
utest diff1 (lam x. if neqf 2. x then 2. else x) 2. with 1. in

-- ltf
utest ltf 1. 1. with false in
utest ltf 1. 0. with false in
utest ltf 0. 1. with true in
utest diff1 (lam x. if ltf x 2. then 2. else x) 1. with 0. in
utest diff1 (lam x. if ltf x 2. then 2. else x) 2. with 1. in
utest diff1 (lam x. if ltf x 2. then 2. else x) 3. with 1. in
utest diff1 (lam x. if ltf 2. x then 2. else x) 1. with 1. in
utest diff1 (lam x. if ltf 2. x then 2. else x) 2. with 1. in
utest diff1 (lam x. if ltf 2. x then 2. else x) 3. with 0. in

-- leqf
utest leqf 1. 1. with true in
utest leqf 1. 0. with false in
utest leqf 0. 1. with true in
utest diff1 (lam x. if leqf x 2. then 2. else x) 1. with 0. in
utest diff1 (lam x. if leqf x 2. then 2. else x) 2. with 0. in
utest diff1 (lam x. if leqf x 2. then 2. else x) 3. with 1. in
utest diff1 (lam x. if leqf 2. x then 2. else x) 1. with 1. in
utest diff1 (lam x. if leqf 2. x then 2. else x) 2. with 0. in
utest diff1 (lam x. if leqf 2. x then 2. else x) 3. with 0. in

-- gtf
utest gtf 1. 1. with false in
utest gtf 1. 0. with true in
utest gtf 0. 1. with false in
utest diff1 (lam x. if gtf x 2. then 2. else x) 1. with 1. in
utest diff1 (lam x. if gtf x 2. then 2. else x) 2. with 1. in
utest diff1 (lam x. if gtf x 2. then 2. else x) 3. with 0. in
utest diff1 (lam x. if gtf 2. x then 2. else x) 1. with 0. in
utest diff1 (lam x. if gtf 2. x then 2. else x) 2. with 1. in
utest diff1 (lam x. if gtf 2. x then 2. else x) 3. with 1. in

-- geqf
utest geqf 1. 1. with true in
utest geqf 1. 0. with true in
utest geqf 0. 1. with false in
utest diff1 (lam x. if geqf x 2. then 2. else x) 1. with 1. in
utest diff1 (lam x. if geqf x 2. then 2. else x) 2. with 0. in
utest diff1 (lam x. if geqf x 2. then 2. else x) 3. with 0. in
utest diff1 (lam x. if geqf 2. x then 2. else x) 1. with 0. in
utest diff1 (lam x. if geqf 2. x then 2. else x) 2. with 0. in
utest diff1 (lam x. if geqf 2. x then 2. else x) 3. with 1. in

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

  let test = lam x. lam dx.
    -- Test binary elementary functions
    utest subf (addf x.0 x.1) x.1 with x.0 in
    utest subf x.0 (addf x.0 x.1) with negf x.1 in
    utest mulf (divf x.0 x.1) x.1 with x.0 using eqfApprox in
    utest mulf x.0 (divf x.1 x.0) with x.1 using eqfApprox in

    let diff1 = lam f : Float -> Float -> Float. lam x : (Float, Float).
      diff (lam a. f a x.1) x.0 1.
    in
    let diff2 = lam f : Float -> Float -> Float. lam x : (Float, Float).
      diff (f x.0) x.1 1.
    in
    let diffTot =
      lam f : Float -> Float -> Float.
        lam x : (Float, Float).
          lam dx : (Float, Float).
            diff (lam x. f x.0 x.1) x dx
    in
    -- addf
    let df = lam x. lam dx. addf dx.0 dx.1 in
    utest diff1 addf x with df x (1., 0.) in
    utest diff2 addf x with df x (0., 1.) in
    utest diffTot addf x dx with df x dx in
    -- mulf
    let df = lam x. lam dx. addf (mulf x.1 dx.0) (mulf x.0 dx.1) in
    utest diff1 mulf x with df x (1., 0.) in
    utest diff2 mulf x with df x (0., 1.) in
    utest diffTot mulf x dx with df x dx in
    -- subf
    let df = lam x. lam dx. subf dx.0 dx.1 in
    utest diff1 subf x with df x (1., 0.) in
    utest diff2 subf x with df x (0., 1.) in
    utest diffTot subf x dx with df x dx in
    -- divf
    let df = lam x. lam dx.
      subf (divf dx.0 x.1) (divf (mulf x.0 dx.1) (mulf x.1 x.1))
    in
    utest diff1 divf x with df x (1., 0.) using eqfApprox in
    utest diff2 divf x with df x (0., 1.) using eqfApprox in
    utest diffTot divf x dx with df x dx using eqfApprox in
    -- pow
    let df = lam x. lam dx.
      addf
        (mulf (mulf x.1 (pow x.0 (subf x.1 1.))) dx.0)
        (mulf
           (if eqf x.0 0. then
             if gtf x.1 0. then 0. else divf 0. 0.
            else
             mulf (pow x.0 x.1) (log x.0))
           dx.1)
    in
    utest diff1 pow x with df x (1., 0.) using eqfApprox in
    -- NOTE(oerikss, 2025-03-01): Make sure to test with something else than
    -- integer exponents.
    (let x = (x.0, addf x.1 0.1) in
     utest diff1 pow x with df x (1., 0.) using eqfApprox in ());
    utest diff2 pow x with df x (0., 1.) using eqfApprox in
    utest diffTot pow x dx with df x dx using eqfApprox in

    -- Test unary elementary functions
    let x = x.0 in
    let dx = dx.0 in
    utest negf (negf x) with x in
    utest exp (log x) with x using eqfApprox in
    utest log (exp x) with x using eqfApprox in
    utest pow (sqrt x) 2. with x using eqfApprox in
    utest sqrt (pow x 2.) with x using eqfApprox in
    -- negf
    let df = lam x. lam dx. negf dx in
    utest diff negf x dx with df x dx in
    -- sin
    let df = lam x. lam dx. mulf (cos x) dx in
    utest diff sin x dx with df x dx in
    -- cos
    let df = lam x. lam dx. negf (mulf (sin x) dx) in
    utest diff cos x dx with df x dx in
    -- exp
    let df = lam x. lam dx. mulf (exp x) dx in
    utest diff exp x dx with df x dx in
    -- log
    let df = lam x. lam dx. divf dx x in
    utest diff log x dx with df x dx in
    -- sqrt
    let df = lam x. lam dx. divf dx (mulf 2. (sqrt x)) in
    utest diff sqrt x dx with df x dx in
    ()
  in

  let rf = lam. int2float (randIntU 1 100) in
  repeat (lam. test (rf (), rf ()) (rf (), rf ())) 100;

  () with () in

()
