/-
Constant-rate birth-death model (CRBD) model
Manual translation from crbd-demo.tppl into CorePPL.
-/

include "tree.mc" -- hard-coded datatype
include "io.mc" -- hard-coded input
include "math.mc" -- because of log

-- Auto-generated projections for .age, .left, and .right
let get_age = lam n: Tree. match n with Node r then r.age else
                 match n with Leaf r then r.age else
                 never

let get_left = lam n: Tree. match n with Node r then r.left else
                 never

let get_right = lam n: Tree. match n with Node r then r.right else
                 never

mexpr 

let flip: Float -> Bool = 
  lam p: Float.
  let e = assume (Bernoulli p) in 
  e
in

recursive
let simulate_side_branch: Float -> Float -> Float -> () =
  lam start_time: Float. lam lambda: Float. lam mu: Float.
  let waiting_time = assume (Exponential (addf lambda mu)) in
  let current_time = subf start_time waiting_time in
  if ltf current_time 0. then
    weight (negf inf);
    ()
  else
    if (flip (divf lambda (addf lambda mu))) then
      simulate_side_branch current_time lambda mu;
      simulate_side_branch current_time lambda mu;
      ()
    else
      ()
in

-- Manual lambda lifting, will not be needed eventuallys
recursive
let _fbdy: Int -> Int -> Tree -> Tree -> Float -> Float -> () =
  lam idx: Int. lam _u: Int. lam child: Tree. lam parent: Tree. lam lambda: Float. lam mu: Float.
  if gti idx _u then () else
    let t = assume (Uniform (get_age child) (get_age parent)) in
    simulate_side_branch t lambda mu;
    weight (log 2.);
    _fbdy (addi idx 1) _u child parent lambda mu
in

-- Simulate tree function -- 64
recursive
let simulate_tree: Tree -> Tree -> Float -> Float -> () =
  lam child: Tree. lam parent: Tree. lam lambda: Float. lam mu: Float.
  
  let k = assume (Poisson (mulf lambda (subf (get_age parent) (get_age child)))) in
 
  -- loop
  -- explicit l to u rather than a sequence (in the momemnt only static)
  let _l = 1 in
  let _u = k in
  /-
  recursive let _fbdy: Int -> () =
    lam idx: Int.
    if gti idx _u then () else
      let t = assume (Uniform (get_age child) (get_age parent)) in
      simulate_side_branch t lambda mu;
      weight (log 2.);
      _fbdy (addi idx 1)
  in
  _fbdy _l;-/
  _fbdy _l _u child parent lambda mu;

  -- observe statement
  observe 0 (Poisson (mulf mu (subf (get_age parent) (get_age child))) ); 

  -- if ... is
  if (match child with Node _ then true else false) then
    observe 0. (Exponential (lambda));
    simulate_tree (get_left child) child lambda mu;
    simulate_tree (get_right child) child lambda mu
  else ()
in


-- Translation of model function on line around 116
-- Instead of proper I/O I have hardcoded it io.mc
let lambda = assume (Gamma k_l t_l) in -- 117
let mu = assume (Gamma k_m t_m) in -- 118
(if (match observation with Node _ then true else false) then
  simulate_tree (get_left observation) lambda mu;
  simulate_tree (get_right observation) lambda mu
else ());

{lambda = lambda, mu = mu} -- line 123
