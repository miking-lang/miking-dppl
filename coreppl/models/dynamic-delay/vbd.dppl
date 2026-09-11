----------------------------------------------------------------------------
-- The SEIR model from https://docs.birch.sh/examples/VectorBorneDisease/ --
----------------------------------------------------------------------------

-- Include pow and exp
include "math.mc"
include "seq.mc"
-- Include data
include "data/data-vbd.mc"

let model: () -> Int = lam.

  -- Human parameters
  let hNu: Float = 0. in
  let hMu: Float = 1. in
  let hLambda = delay (Beta 1. 1.) in
  let hDelta =
    delay (Beta (addf 1. (divf 2. 4.4)) (subf 3. (divf 2. 4.4))) in
  let hGamma =
    delay (Beta (addf 1. (divf 2. 4.5)) (subf 3. (divf 2. 4.5))) in

  -- Mosquito parameters
  let mNu: Float = divf 1. 7. in
  let mMu: Float = divf 6. 7. in
  let mLambda = delay (Beta 1. 1.) in
  let mDelta =
    delay (Beta (addf 1. (divf 2. 6.5)) (subf 3. (divf 2. 6.5))) in
  let mGamma: Float = 0. in

  -- Other parameters
  let rho = delay (Beta 1. 1.) in
  let z: Int = 0 in

  -- Human SEIR component
  let n: Int = 7370 in
  let hI: Int = addi 1 (assume (Poisson 5.0)) in
  let hE: Int = assume (Poisson 5.0) in
  let hR: Int =
    floorfi (assume (Uniform 0. (int2float (addi 1 (subi (subi n hI) hE))))) in
  let hS: Int = subi (subi (subi n hE) hI) hR in

  -- Human initial deltas
  let hDeltaS: Int = 0 in
  let hDeltaE: Int = hE in
  let hDeltaI: Int = hI in
  let hDeltaR: Int = 0 in

  -- Mosquito SEIR component
  let u: Float = assume (Uniform (negf 1.) 2.) in
  let mS: Int = floorfi (mulf (int2float n) (pow 10. u)) in
  let mE: Int = 0 in
  let mI: Int = 0 in
  let mR: Int = 0 in

  -- Mosquito initial deltas
  let mDeltaS: Int = 0 in
  let mDeltaE: Int = 0 in
  let mDeltaI: Int = 0 in
  let mDeltaR: Int = 0 in

  -- Conditioning function
  let condition: Int -> Int -> Int -> Int =
    lam t: Int. lam zP: Int. lam hDeltaI: Int.
      let z: Int = addi zP hDeltaI in
      let y: Int = get ys t in
      let z: Int = if neqi (negi 1) y then
        if geqi z y then
          let rho = delayed rho in
          observe y (Binomial z rho); 0
        else weight (negf inf); 0
        else z in
      z
  in

  -- Initial conditioning
  let z = condition 0 z hDeltaI in

  -- Simulation function
  recursive let simulate:
    Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
    =
    lam t: Int.
    lam hSP: Int.
    lam hEP: Int.
    lam hIP: Int.
    lam hRP: Int.
    lam mSP: Int.
    lam mEP: Int.
    lam mIP: Int.
    lam mRP: Int.
    lam zP: Int.

      -- Humans
      let hN: Int = addi (addi (addi hSP hEP) hIP) hRP in
      let hTau: Int =
        assume (Binomial hSP (subf 1. (exp (negf (divf
                                                  (int2float mIP)
                                                  (int2float hN)))))) in
      let hLambda = delayed hLambda in
      let hDeltaE: Int = assume (Binomial hTau hLambda) in
      let hDelta = delayed hDelta in
      let hDeltaI: Int = assume (Binomial hEP hDelta) in
      let hGamma = delayed hGamma in
      let hDeltaR: Int = assume (Binomial hIP hGamma) in
      let hS: Int = subi hSP hDeltaE in
      let hE: Int = subi (addi hEP hDeltaE) hDeltaI in
      let hI: Int = subi (addi hIP hDeltaI) hDeltaR in
      let hR: Int = addi hRP hDeltaR in

      -- Mosquitos
      let mTau: Int =
        assume (Binomial mSP (subf 1. (exp (negf (divf
                                                 (int2float hIP)
                                                 (int2float hN)))))) in
      let mN: Int  = addi (addi (addi mSP mEP) mIP) mRP in
      let mLambda = delayed mLambda in
      let mDeltaE: Int = assume (Binomial mTau mLambda) in
      let mDelta = delayed mDelta in
      let mDeltaI: Int = assume (Binomial mEP mDelta) in
      let mDeltaR: Int = assume (Binomial mIP mGamma) in
      let mS: Int = subi mSP mDeltaE in
      let mE: Int = subi (addi mEP mDeltaE) mDeltaI in
      let mI: Int = subi (addi mIP mDeltaI) mDeltaR in
      let mR: Int = addi mRP mDeltaR in

      let mS: Int = assume (Binomial mS mMu) in
      let mE: Int = assume (Binomial mE mMu) in
      let mI: Int = assume (Binomial mI mMu) in
      let mR: Int = assume (Binomial mR mMu) in

      let mDeltaS: Int = assume (Binomial mN mNu) in
      let mS: Int = addi mS mDeltaS in

      let z: Int = condition t zP hDeltaI in

      -- Recurse
      let tNext: Int = addi t 1 in
      if eqi (length ys) tNext then
        -- We do not return anything here, but simply run the model for
        -- estimating the normalizing constant. The '0' return value makes the
        -- program compatible with RootPPL (for which we cannot return ()).
        0
      else
        simulate tNext hS hE hI hR mS mE mI mR z
  in

  -- Initiate recursion
  simulate 1 hS hE hI hR mS mE mI mR z

mexpr
model ()
