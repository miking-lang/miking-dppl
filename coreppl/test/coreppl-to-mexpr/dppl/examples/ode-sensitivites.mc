include "../lib.mc"
include "../lotka-model.mc"

let join = lam xs : [[FloatA]]. foldl (lam y : [FloatA]. concat y) [] xs

-- Specialize solveode
let solve = lam f : [FloatA] -> [FloatA]. lam y0 : [FloatA]. lam dxs : [FloatP].
  let s = lam f : [FloatA] -> [FloatA]. lam y0 : [FloatA]. lam x : FloatP.
    solveode (Default { add = adds, smul = smuls, stepSize = 1e-3 })
      (lam x : FloatA. f) y0 x
  in
  reverse
    (foldl (lam ys : [[FloatA]]. lam dx : FloatP. cons (s f (head ys) dx) ys)
       [y0] dxs)

let sns = lam #var"θ" : FloatA. lam n : Int. lam dx : FloatP.
  -- Times-steps
  let dxs = create n (lam i : Int. dx) in

  -- Function that takes a parametrized ODE model as input and
  -- outputs a ODE model augmented with sensitivity equations.
  let augmentSens = lam f : [FloatA] -> [FloatA] -> [FloatA]. lam ny : Int.
    lam #var"θ" : [FloatA]. lam y : [FloatA].
      if neqi (muli (addi (length #var"θ") 1) ny) (length y) then
        error
          (concat "augmentSens: incorrect number of states: "
             (float2string (int2float (length y))))
      else
      -- Group the dependent variables:
      --  y = [y_1, .., y_n,
      --       (dydθ_1)_1, .., (dydθ_1)_ny,
      --       ..,
      --       (dydθ_nθ)_1, .., (dydθ_nθ)_ny]
      -- into:
      --   [[y_1, .., y_ny],
      --    [(dydθ_1)_1, .., (dydθ_1)_ny],
      --    ..,
      --    [(dydθ_nθ)_1, .., (dydθ_nθ)_ny]]
      let ys =
        foldl
          (lam acc : [[FloatA]]. lam y : FloatA.
            switch acc
            case [] then error "impossible"
            case yss ++ [ys] then
              if lti (length ys) ny then snoc yss (snoc ys y)
              else snoc acc [y]
            end)
          [[]]
          y
      in
      match ys with [y] ++ sns in
      concat
        -- original ODE model: f(x), in x' = f(x).
        (f #var"θ" y)
        (join
           (mapi
              (lam i : Int. lam sns : [FloatA].
                -- sensitivity model: (dx/dθ_i)' = (df/dx)(dx/dθ_i) + df/dθ_i
                adds
                  (diffA (f #var"θ") y sns)
                  (diffA (lam #var"θ" : [FloatA]. f #var"θ" y)
                     #var"θ" (onehots (length #var"θ") i)))
              sns))
  in

  let lotkaVolterra = lam #var"θ" : FloatA. lotkaVolterra (#var"θ", 1., 1., 3.) in

  -- We parametrize our ODE model on the prey per-capita growth rate.
  let ode1 = lam #var"θ" : [FloatA]. lam y : [FloatA].
    match #var"θ" with [#var"θ"] in
    match lotkaVolterra #var"θ" (get y 0, get y 1) with (f1, f2) in
    [f1, f2]
  in

  -- Initial values
  let y0 = [1., 1.] in

  -- The number of dependent variables
  let ny = length y0 in

  -- We also create a second ODE model that includes the sensitivities directly.
  let ode2 = augmentSens ode1 ny in

  -- Trace the sensitivites w.r.t. the parameter by differentiating the solution
  -- trace.
  let sns1 = lam dx : [FloatA].
    diffA (lam #var"θ" : [FloatA]. solve (ode1 #var"θ") y0 dxs) [#var"θ"] dx
  in

  -- We also compute the forward continous sensitvities.
  let sns2 =
    map
      (lam y : [FloatA].
        match y with [_, _, s1, s2] then [s1, s2] else error "impossible")
      (solve (ode2 [#var"θ"]) (concat y0 [0., 0.]) dxs)
  in

  -- Return the distribution of the two traces
  (sns1 [1.], sns2)

mexpr

-- Number of time-steps
let n = 100 in
-- Size time-step
let dx = 0.1 in
let xs = create (addi n 1) (lam i : Int. mulf (int2float i) dx) in

let model = lam t : ().
  -- Uniformly draw the model parameter
  let #var"θ" = assume (Uniform 1.1 1.5) in
  match sns #var"θ" n dx with (sns1, sns2) in
  [sns1, sns2]
in
let snsDist = infer (Importance { particles = 100 }) model in
match distEmpiricalSamples snsDist with (samples, weights) in
writeFile "ode-sensitivites.json"
  (jsonObject [
    ("xs", seqToJson (map floatToJson xs)),
    ("samples",
     seqToJson (map (lam x : [[[FloatN]]].
       seqToJson (map (lam x : [[FloatN]].
         seqToJson (map (lam x : [FloatN].
           seqToJson (map floatToJson x)) x)) x)) samples))
  ]);

-- Compare the sensitivites from the two methods
match sns 1.1 n dx with (sns1, sns2) in
utest join sns1 with join sns2 using eqs in

()
