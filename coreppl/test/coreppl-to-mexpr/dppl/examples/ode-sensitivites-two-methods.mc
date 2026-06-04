include "../lib.mc"
include "../lotka-model.mc"

-- Specialize solver
let solve =
  lam f : FloatA -> ModC (ModA ([[FloatA]] -> ModC (ModA [[FloatA]]))).
    lam xy0 : (FloatA, [[FloatA]]).
      lam x : FloatPC.
        solveode
          (RK4EC
            { stepSize = 1e-2
            , add =
              lam as : [[FloatA]]. lam bs : [[FloatA]].
                mapi (lam i : Int. lam a : [FloatA]. adds a (get bs i)) as
            , smul = lam s : FloatA. lam as : [[FloatA]]. map (smuls s) as
            , ok =
              lam yh : [[Float]]. lam y2h2 : [[Float]].
                let yh =
                  foldl (lam acc : [Float]. lam yh : [Float]. concat acc yh) [] yh in
                let y2h2 =
                  foldl
                    (lam acc : [Float]. lam y2h2 : [Float]. concat acc y2h2)
                    []
                    y2h2 in
                ltf (l2norms (subs yh y2h2)) (mulf (int2float (length yh)) 1e-2)
            })
          f xy0 x

-- Creates a solution trace from a IVP solution
let trace =
  lam y : (FloatA, [[FloatA]]) -> ModA (FloatPC -> ModC (ModP (ModA (FloatA, [[FloatA]])))).
    lam xy0 : (FloatA, [[FloatA]]).
      lam xs : [FloatPC].
        tail
          (reverse
             (foldl
                (lam xys : [(FloatA, [[FloatA]])]. lam x : FloatPC.
                  cons (y (head xys) x) xys)
                [xy0] xs))


let _n = 200
let _h = 0.05
let times = create _n (lam i : Int. mulf _h (int2float (addi i 1)))

-- Initial values
let x0 = 0.
let y0 = [1., 1.]

-- ODE model
let ode = lam #var"θ" : [FloatA]. lam x : FloatA. lam y : [FloatA].
  let t =
    lotkaVolterra
      (get #var"θ" 0, get #var"θ" 1, get #var"θ" 2, get #var"θ" 3)
      (get y 0, get y 1) in
  [t.0, t.1]

-- IVP solution
let y = lam #var"θ" : [FloatA]. lam xy0 : (FloatA, [[FloatA]]). lam x : FloatPC.
  solve (lam x : FloatA. lam ys : [[FloatA]]. [ode #var"θ" x (head ys)]) xy0 x

let f = ode

-- The righthand side of the j'th sensitivity equation
let #var"fSⱼ" =
  lam f : [FloatA] -> FloatA -> ModA ([FloatA] -> ModA [FloatA]).
    lam #var"θ" : [FloatA]. lam x : FloatA. lam y : [FloatA].
      lam j : Int. lam #var"dy/dθⱼ" : [FloatA].
        let #var"df/dy⋅dy/dθⱼ" = diff (f #var"θ" x) y #var"dy/dθⱼ" in
        let #var"eⱼ" = onehots (length #var"θ") j in
        let #var"df/dθⱼ" =
          diff (lam #var"θ" : [FloatA]. f #var"θ" x y) #var"θ" #var"eⱼ"
        in
        adds #var"df/dy⋅dy/dθⱼ" #var"df/dθⱼ"

-- The original ODE right-hand side augmented with all sensitivity equations
let fS =
  lam f : [FloatA] -> FloatA -> ModA ([FloatA] -> ModA [FloatA]).
    lam #var"θ" : [FloatA]. lam x : FloatA. lam ys : [[FloatA]].
      match ys with [y] ++ #var"dy/dθ" then
        cons
          (f #var"θ" x y)
          (mapi (#var"fSⱼ" f #var"θ" x y) #var"dy/dθ")
      else error "invalid input"

-- IVP solution with sensitivities
let yS = lam #var"θ" : [FloatA]. lam xy0 : (FloatA, [[FloatA]]). lam x1 : FloatPC.
  solve (fS f #var"θ") xy0 x1

let _model = lam t : ().
  let #var"θ₁" = assume (Uniform 1.0 2.0) in
  let #var"θ" = [#var"θ₁", 1., 1., 3.] in
  let #var"nθ" = length #var"θ" in
  let j = 0 in                  -- choose sensitivity 0 to 3
  let yS0 =
    cons
      y0
      (create #var"nθ"
         (lam i: Int.
           create (length y0)
             (lam j : Int. 0.))) in
  [
    map
      (lam t : (FloatA, [[FloatA]]). (t.0, get t.1 (addi 1 j)))
      (trace (yS #var"θ") (x0, yS0) times),
    map
      (lam t : (FloatA, [[FloatA]]). (t.0, get t.1 0))
      (diff
         (lam #var"θ" : [FloatA]. trace (y #var"θ") (x0, [y0]) times)
         #var"θ"
         (onehots #var"nθ" j))
  ]

let #var"Dist_dy/dθ" = infer (Importance { particles = 100 }) _model

mexpr

match distEmpiricalSamples #var"Dist_dy/dθ" with (samples, weights) in

-- Assert that the solutions are equal, not identically zero, and that they have
-- reached the expected time.
iter
  (lam x : [[(Float, [Float])]].
    match x with [s1, s2] then
      iteri
        (lam i : Int. lam t : (Float, [Float]).
          utest t.0 with get times i using eqfApprox 1.e-15 in
          iteri
            (lam j : Int. lam yj : Float.
              (if neqi i 0 then utest eqf yj 0. with false in () else ());
              utest yj with get (get s2 i).1 j using eqfApprox 1.e-12 in ())
            t.1;
          ())
        s1
    else error "impossible")
  samples;

let samples =
  map
    (lam x : [[(Float, [Float])]].
      map
        (lam ts : [(Float, [Float])].
          mapi
            (lam i : Int. lam t : (Float, [Float]). (get times i, t.1))
            ts)
        x)
    samples in
printWeightedTraces samples weights

-- local variables:
-- compile-command: "cppl --test --seed 1 --cps partial --dppl-typecheck ode-sensitivites-two-methods.mc && ./out | dppl-plot-process --lines && rm ./out"
-- End:
