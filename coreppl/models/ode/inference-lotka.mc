include "./ode-common.mc"
include "./lotka-model.mc"

-- Number of time-steps
let n = 100
-- Size time-step
let dt = 0.1
-- Time-steps
let dts = create n (lam. dt)

-- We parametrize our ODE model on the prey per-capita growth rate.
let ode = lam p. lam xs.
  match lotkaVolterra (p, 1., 1., 3.) (get xs 0, get xs 1) with (f1, f2) in
  [f1, f2]

-- We can only observe the preys in our model
let odeObserve = lam xs. get xs 0

-- We construct a parametrized solution trace from our ODE mode.
let trace = lam p. map odeObserve (odeTrace (lam. lam xs. ode p xs) [1., 1.] dts)

-- True parametere value
let p = 1.5

-- Measurement noise
let sigma = 0.2

-- The system is the output trace with the true parameter with added measurement
-- noise.
let system = lam.
  map (lam y. let v = assume (Gaussian 0. sigma) in addf y v) (trace p)

-- Genereate new data by drawing one sample from the system. The following data
-- use sigma = 0.2.
let data = [1.08031492294,1.12318304667,0.847496932043,1.14050788877,1.35222496823,1.53745426155,2.17130541567,1.95313820393,1.9547110161,2.26029735801
-- ,2.96786845262,3.3291466847,3.27379657045,3.97046297145,4.71429632178,5.29958296545,5.6006136419,6.29487727331,6.55789385065,6.96533574475,7.01762580277,6.14981340266,5.16613862516,4.10509620233,3.19515150102,2.02339126225,1.85804313571,1.45203575633,1.10941482144,1.19304765355,0.891171785064,0.935188979791,0.944252118028,0.547104802192,0.758564354247,1.16675177475,1.20505331544,1.48566813126,1.59682011967,2.03792548964,1.82027119722,2.44899171295,2.56310428983,2.81412772645,3.33383592327,3.36528178868,4.00924895258,4.49143372651,5.35491051256,5.58058854819,5.79786792307,6.74654736746,7.01011617574,7.06103989717,6.16810748575,5.44121772966,4.22426370205,3.17543409387,2.5323460716,1.80915222468,1.13427848638,1.11212123055,0.832044165108,0.993509074129,0.550075849582,0.657448300842,0.881433259852,1.16420484217,1.20959110384,1.5110383744,1.52477098915,1.84907991506,1.75781594004,1.69417013149,2.31106235466,2.32392599748,2.71941760767,3.30249233144,3.50500892246,3.75765603381,4.64609811168,5.2091235511,5.09458285857,6.105706635,6.38847207872,6.87103557462,6.80185777008,6.17833039671,5.59724204538,4.23144098056,3.68225723849,2.57647650633,2.10519190318,1.61194360741,1.07456308041,1.27279883343,1.41005798054,1.07636289703,1.02065095036,0.88257679394,0.877031789279
]

let model = lam data. lam.
  -- our prior belief of the true parameter value and measurement error
  let p = assume (Uniform 0. 5.) in
  let sigma = assume (Uniform 0. 1.) in

  -- solve our ODE model and get its output with the given parameter.
  let ys = trace p in

  -- condition the solution trace on the data
  iter
    (lam t. match t with (y, yhat) in observe y (Gaussian yhat sigma))
    (zip data ys);

  -- We are interested in the posterior distribution over the parameter value
  p

mexpr
let p = infer (APF { particles = 1000 }) (model data) in
printDist printFloat p

-- Comment the two lines above and uncomment the line below to generate new data
-- to standard out.
-- printDist (printSeq printFloat) (infer (Importance { particles = 1 }) system)
