/-
  This example programs illustrates the use of infer and solveode to estimate an
  unknown parameter of an ODE in a Baysian setting.
-/

include "../lib.mc"
include "../lotka-model.mc"

-- Specialize solveode
let solve =
  lam f : FloatA -> (FloatA, FloatA) -> (FloatA, FloatA) .
    lam y0 : (FloatA, FloatA).
      lam x : FloatP.
        solveode (RK4 {stepSize = 1e-3, add = addp, smul = smulp }) f y0 x

-- Size time-step
let hData = 0.1

-- ODE model
let ode =
  lam #var"θ" : FloatA.
    lam x : FloatA.
      lam y : (FloatA, FloatA).
        lotkaVolterra (#var"θ", 1., 1., 3.) y

-- We can only observe the preys in our model
let output = lam y : (FloatA, FloatA). y.0

-- Initial values
let y0 = (1., 1.)

-- True parametere value
let #var"trueθ" = 1.5

-- Measurement noise
let #var"trueσ" = 0.2

-- Genereate new data by drawing one sample from the system. The following data
-- use #var"trueσ" = 0.2.
let data = [
  1.08031492294,
  1.12318304667,
  0.847496932043,
  1.14050788877,
  1.35222496823,
  1.53745426155,
  2.17130541567,
  1.95313820393,
  1.9547110161,
  2.26029735801,
  2.96786845262,
  3.3291466847,
  3.27379657045,
  3.97046297145,
  4.71429632178,
  5.29958296545,
  5.6006136419,
  6.29487727331,
  6.55789385065,
  6.96533574475,
  7.01762580277,
  6.14981340266,
  5.16613862516,
  4.10509620233,
  3.19515150102,
  2.02339126225,
  1.85804313571,
  1.45203575633,
  1.10941482144,
  1.19304765355,
  0.891171785064,
  0.935188979791,
  0.944252118028,
  0.547104802192,
  0.758564354247,
  1.16675177475,
  1.20505331544,
  1.48566813126,
  1.59682011967,
  2.03792548964,
  1.82027119722,
  2.44899171295,
  2.56310428983,
  2.81412772645,
  3.33383592327,
  3.36528178868,
  4.00924895258,
  4.49143372651,
  5.35491051256,
  5.58058854819,
  5.79786792307,
  6.74654736746,
  7.01011617574,
  7.06103989717,
  6.16810748575,
  5.44121772966,
  4.22426370205,
  3.17543409387,
  2.5323460716,
  1.80915222468,
  1.13427848638,
  1.11212123055,
  0.832044165108,
  0.993509074129,
  0.550075849582,
  0.657448300842,
  0.881433259852,
  1.16420484217,
  1.20959110384,
  1.5110383744,
  1.52477098915,
  1.84907991506,
  1.75781594004,
  1.69417013149,
  2.31106235466,
  2.32392599748,
  2.71941760767,
  3.30249233144,
  3.50500892246,
  3.75765603381,
  4.64609811168,
  5.2091235511,
  5.09458285857,
  6.105706635,
  6.38847207872,
  6.87103557462,
  6.80185777008,
  6.17833039671,
  5.59724204538,
  4.23144098056,
  3.68225723849,
  2.57647650633,
  2.10519190318,
  1.61194360741,
  1.07456308041,
  1.27279883343,
  1.41005798054,
  1.07636289703,
  1.02065095036,
  0.88257679394,
  0.877031789279
]
-- let data = create 12 (lam i : Int. (mulf hData (int2float i), get data i))
let data = create 11 (lam i : Int. (get data i))

-- Probabilistic model
let model = lam t : ().
  -- Unknown parameter
  let #var"θ" = assume (Gaussian 1. 1.) in
  -- Measurement noise
  let #var"σ" = assume (Beta 2. 2.) in
  foldl
    (lam y : (FloatN, FloatN). lam o : FloatN.
      observe o (Gaussian (output y) #var"σ");
      solve (ode #var"θ") y hData)
    y0
    data;
  #var"θ"

mexpr

let printDist = false in

let eq = eqfApprox 0.05 in

let #var"θDist" = infer (APF { particles = 1000 }) model in
match distEmpiricalSamples #var"θDist" with (samples, weights) in
writeFile "baysian-parameter-estimation.json"
  (jsonObject [
    ("weights", (seqToJson (map floatToJson weights))),
    ("samples", (seqToJson (map floatToJson samples))),
    ("mean", floatToJson (expectation #var"θDist"))
  ]);

let #var"θ" = expectation #var"θDist" in

utest #var"θ" with #var"trueθ" using eq in

let eq = eqfApprox 0.1 in
let x = 10. in

(if printDist then printFloatDist #var"θDist" else ())
