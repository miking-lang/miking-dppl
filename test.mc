-- mexpr
-- external externalLog : Float -> Float in
-- let log = lam x. externalLog x in

-- recursive let logFactorial = lam n.
--   if eqi n 1 then 0.0 else
--   addf (log (int2float n)) (logFactorial (subi n 1)) in

-- let f = lam n. logFactorial n in


-- let thing = log 10.0 in
-- weight (subf (f 3) (log 3.0));
-- ()

include "math.mc"
include "bool.mc"

type Tree
con Node: {left: Tree, right: Tree, age: Float} -> Tree
con Leaf: {age: Float} -> Tree

recursive let logFactorial = lam n.
  if eqi n 1 then 0. else addf (log (int2float n)) (logFactorial (subi n 1))
end

recursive let countLeaves = lam tree.
  match tree with Node r then addi (countLeaves r.left) (countLeaves r.right) else 1
end

let getAge = lam n. match n with Node r then r.age else match n with Leaf r then r.age else never

let clads2: Tree -> Float -> Float = lam tree. lam rho.

  -- Priors
  --let lambda = assume (Exponential 1.0) in
  --let mu = assume (Uniform 0.0 lambda) in
  --let sigma = 0.0 in
  --let log_alpha = 0.0 in

  --let lambda = 0.12 in
  --let mu = 0.06 in
  --let sigma = 0.0 in
  --let log_alpha = 0.0 in

  let lambda = assume (Gamma 1.0 1.0) in
  let mu = assume (Gamma 1.0 0.5) in
  let sigma = sqrt (divf 0.2 (assume (Gamma 1.0 1.0))) in
  let log_alpha = assume (Gaussian 0. sigma) in

  recursive let survives = lam tBeg. lam multiplier.
    let multiplier = mulf multiplier (exp (assume (Gaussian log_alpha sigma))) in
    if or (ltf multiplier 1e-5) (gtf multiplier 1e5) then
      true
    else
      let t = subf tBeg (assume (Exponential (mulf multiplier (addf lambda mu)))) in
      if ltf t 0. then
        assume (Bernoulli rho)
      else
        if assume (Bernoulli (divf lambda (addf lambda mu))) then
          -- Speciation
          if survives t multiplier then
            true
          else
            survives t multiplier
        else
          -- Extinction
          false
  in

  recursive let simHiddenSpeciation = lam nodeAge. lam tBeg. lam multiplier.
    let multiplier = mulf multiplier (exp (assume (Gaussian log_alpha sigma))) in
    if or (ltf multiplier 1e-5) (gtf multiplier 1e5) then
      weight (negf inf);
      multiplier
    else
      let t = subf tBeg (assume (Exponential (mulf multiplier lambda))) in
      if gtf t nodeAge then
        if survives t multiplier then
          weight (negf inf);
          multiplier
        else
          observe 0 (Poisson (mulf (mulf mu multiplier) (subf tBeg t)));
          weight (log 2.);
          simHiddenSpeciation nodeAge t multiplier
      else
        observe 0 (Poisson (mulf (mulf mu multiplier) (subf tBeg nodeAge)));
        multiplier
  in

  recursive let walk = lam node. lam parentAge. lam multiplier.
    let nodeAge = getAge node in
    let multiplier = simHiddenSpeciation nodeAge parentAge multiplier in
    match node with Node n then
      observe 0. (Exponential (mulf multiplier lambda));
      walk n.left nodeAge multiplier;
      walk n.right nodeAge multiplier
    else match node with Leaf _ then
      observe true (Bernoulli rho)
    else never
  in

  let numLeaves = countLeaves tree in
  weight (subf (mulf (subf (int2float numLeaves) 1.) (log 2.)) (logFactorial numLeaves));
  match tree with Node root in
  walk root.left root.age 1.;
  walk root.right root.age 1.;
  lambda


-- Synthetic
-- let tree = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 4.862406452197589}, right = Node {left = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 0.302169162393619}, right = Leaf {age = 0.0}, age = 1.2350618429801936}, right = Leaf {age = 0.0}, age = 2.0866037802290065}, age = 5.0}
-- let rho = 1.0

-- Alcedinidae
let tree = Node {left = Node {left = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 5.635787971}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 4.788021775}, age = 7.595901077}, age = 9.436625313}, age = 12.344087935000001}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 3.934203877}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 3.151799953}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 5.054547857}, age = 6.284896356999999}, age = 7.815689970999999}, age = 10.32243059}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 1.519406055}, age = 4.987038163}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 0.6302632958}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 1.962579854}, age = 3.732932004}, age = 5.5933070698}, age = 6.096453021}, age = 8.265483252}, age = 10.86835485}, age = 12.551924091}, age = 13.472886809}, right = Node {left = Node {left = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 4.534421013}, age = 12.46869821}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 6.306427821}, age = 9.40050129}, age = 13.85876825}, age = 20.68766993}, age = 22.82622451}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 4.220057646}, age = 8.451051062}, age = 11.54072627}, age = 15.28839572}, right = Node {left = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 8.614086751}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 0.9841688636}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 1.04896206}, age = 1.7140599232}, age = 3.786162534}, age = 8.788450495}, age = 11.05846217}, age = 15.008504768}, right = Node {left = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 11.15685875}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Node {left = Leaf {age = 0.0}, right = Leaf {age = 0.0}, age = 1.900561313}, age = 3.100150132}, age = 6.043650727}, age = 12.38252513}, age = 12.61785812}, age = 15.396725736}, age = 16.828404506}, age = 20.368109703000002}, age = 23.74299959}, age = 32.145876657}, age = 34.940139089}
let rho = 0.5684210526315789
mexpr
clads2 tree rho
