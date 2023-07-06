---------------------------------------------------
-- A state-space model for aircraft localization --
---------------------------------------------------

include "math.mc"

-- Noisy satellite observations of position (accuracy is improved
-- at higher altitude)
let data: [Float] = [
  603.57, 860.42, 1012.07,
  1163.53, 1540.29, 1818.10,
  2045.38, 2363.49, 2590.77,
  2801.91
]

let model: () -> Float = lam.

  let holdingAltitude = 35000. in
  let altitudeRange = 100. in
  let position: Float = assume (Uniform 0. 1000.) in
  let altitude: Float = assume (Gaussian holdingAltitude 200.) in

  let positionStDev = 50. in

  let baseVelocity = 250. in
  let velocity: Float -> Float = lam altitude.
    let k = divf baseVelocity holdingAltitude in
    minf 500. (maxf 100. (mulf k altitude))
  in

  let basePositionObsStDev = 50. in
  let positionObsStDev: Float -> Float = lam altitude.
    let m = 100. in
    let k = negf (divf basePositionObsStDev holdingAltitude) in
    maxf 10. (addf m (mulf k altitude))
  in

  let altitudeStDev = 100. in

  recursive let simulate: Int -> Float -> Float -> Float =
    lam t: Int. lam position: Float. lam altitude: Float.

    -- Observe position
    let dataPos: Float = get data t in
    observe dataPos (Gaussian position (positionObsStDev altitude));
    -- resample; -- Should be added by both aligmnent analysis and likelihood resampling
    let t = addi t 1 in

    -- Penalize altitude divergence of more than `altitudeRange` feet from
    -- holding altitude
    (if gtf (absf (subf altitude holdingAltitude)) altitudeRange then
       weight (log 0.5)--; resample -- Should only be added by likelihood resampling
     else ());

    -- Transition
    let position: Float =
      assume (Gaussian (addf position (velocity altitude)) positionStDev) in
    let altitude: Float = assume (Gaussian altitude altitudeStDev) in

    if eqi (length data) t then position
    else simulate t position altitude
  in

  simulate 0 position altitude

mexpr
model ()

