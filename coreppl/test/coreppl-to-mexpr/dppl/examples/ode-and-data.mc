/- ODE model and data -/

include "../lib.mc"
include "../lotka-model.mc"

-- Specialize solver
let solve =
  lam f : FloatA -> ModC (ModA ([FloatA] -> ModC (ModA [FloatA]))).
    lam xy0 : (FloatA, [FloatA]).
      lam x : FloatPC.
        solveode
          (RK4EC
            { stepSize = 1e-2
            , add = adds
            , smul = smuls
            , ok =
              lam yh : [Float]. lam y2h2 : [Float].
                ltf (l2norms (subs yh y2h2)) (mulf (int2float (length yh)) 1e-2)
            })
          f xy0 x

-- Creates a solution trace from a IVP solution
let trace =
  lam y : (FloatA, [FloatA]) -> ModA (FloatPC -> ModC (ModP (ModA (FloatA, [FloatA])))).
    lam xy0 : (FloatA, [FloatA]).
      lam xs : [FloatPC].
        tail
          (reverse
             (foldl
                (lam xys : [(FloatA, [FloatA])]. lam x : FloatPC.
                  cons (y (head xys) x) xys)
                [xy0] xs))

-- Initial values
let x0 = 0.
let y0 = [1., 1.]

-- ODE model
let ode = lam #var"θ" : FloatA. lam x : FloatA. lam y : [FloatA].
  let t = lotkaVolterra (#var"θ", 1., 1., 3.) (get y 0, get y 1) in
  [t.0, t.1]

-- IVP solution
let y = lam #var"θ" : FloatA. lam xy0 : (FloatA, [FloatA]). lam x : FloatPC.
  solve (ode #var"θ") xy0 x

-- We can only observe the density of preys
let g = lam t : (FloatA, [FloatA]). get t.1 0

-- Size time-step
let hData = 0.1

-- True parametere value
let #var"true_θ" = 1.5

-- True solution
let true_y = lam xy0 : (FloatA, [FloatA]). lam x : FloatPC.
  solveode
    (RK4EC
      { stepSize = 1e-3
      , add = adds
      , smul = smuls
      , ok =
        lam yh : [Float]. lam y2h2 : [Float].
          ltf (l2norms (subs yh y2h2)) (mulf (int2float (length yh)) 1e-2)
      })
    (ode #var"true_θ")  xy0 x

-- Measurement noise ν ~ N(0, 1)
let noise = [ -0.82890921, -0.25021182, -0.63268277, -0.48174128,  1.36814387,
              -0.3382801 , -1.67044097,  0.23290733,  0.40381896,  0.56071326,
              -0.76638514, -0.36183319, -0.57863078,  0.4482448 ,  0.66146646,
               0.14992744, -0.0350686 ,  0.52786751,  0.98155723, -0.9502356 ,
               1.51069788,  1.54892217, -0.65113557,  0.79417729,  1.32780553,
               0.14370155,  0.73066445,  0.69398384, -1.20767714, -2.28099253,
               1.24221878, -0.96819056, -1.27312482, -0.7500186 , -2.8709592 ,
               0.59924282,  2.41892407,  2.0813987 ,  1.9328879 , -0.12883221,
               1.16456546,  1.25951108,  0.00287253, -0.21623207, -1.84158825,
              -0.10077107, -2.35427482,  1.20177156,  1.16659711,  0.6363899 ,
               0.59003973, -1.24242733,  0.08208246, -1.31205623,  0.31736578,
              -0.9134139 ,  0.00330399,  0.60013721, -0.13093799, -0.23675927,
               0.11204715, -1.12947555,  0.09678479, -1.16814036,  1.22878837,
              -0.59715526, -1.64888967,  0.54026557,  0.59349749,  0.36012143,
               0.4166738 ,  0.06333757,  1.71516971, -0.58145004, -2.29586845,
               0.19010883,  1.14099624, -0.25375215, -1.02807143,  0.66623493,
              -0.86112375,  0.37977746, -1.22156755, -0.61426151,  2.47919245,
               0.617599  , -0.28158577,  1.58944193,  0.98776377,  1.67448668,
               0.96408363,  1.09987872,  0.08669379,  1.05713146, -0.66545296,
               0.52283111,  0.61430532,  1.69428908,  0.73104413,  0.04681288 ]

let times = create 14 (lam i : Int. mulf hData (int2float (addi i 1)))

let data =
  mapi
    (lam i : Int. lam xy : (FloatA, [FloatA]). addf (g xy) (get noise i))
    (trace true_y (x0, y0) times)

let future = 10.

mexpr

let m = lam t : (). () in
infer (Importance { particles = 2 }) m;
()
