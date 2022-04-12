------------------------------------------------------------------------------------
-- Constant-rate birth-death model (CRBD) model                                   --
-- Direct translation from TreePPL                                                --
------------------------------------------------------------------------------------

-- Tree type definition, lines 14 - 16
type Tree -- 14
con Leaf : { age : Float, index: Int } -> Tree -- 15
con Node : { left : Tree, right : Tree, age : Float, index: Int } -> Tree -- 16

-- Project the age from a tree
-- because of child.age and parent.age -- 78
let getAge = lam n: Tree. match n with Node r then r.age else
                 match n with Leaf r then r.age else
                 never

-- "Hardcoding" input and output for now
include "io.mc" -- 116

mexpr -- Not sure where to put this? All programs seem to have it.

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
  if current_time < 0 then
    weight (negf inf) in ()
  else
    if (flip (divf lambda (addf lambda mu))) then
      simulate_side_branch current_time lambda mu;
      simulate_side_branch current_time lambda mu;
      ()
    else
    ()
in

-- Simulate tree function -- 64
recursive let simulate_tree: Tree -> Tree -> Float -> Float -> () =
  lam tree: Tree.
  lam parent: Tree.
  lam lambda: Float.
  lam mu: Float.
  
  let k = assume (Poisson (mulf lambda (subf (getAge parent) (getAge child)))) in
  -- loop lines 77 - 81 
  -- A loop is defined a sequence of statements over a range.
  -- It does not return anything but process the elements of
  -- the range in order.
  -- Define range over which we will iterate 1..k
  -- how many elements
  let _l = 1 in
  let _u = k in
  let _n = subi _u (subi _l 1) in
  let _rng = create n (lam i. addi 1 i) in
  let _fbdy: a -> () =
    lam _rlmnt: a.
    let t = assume (Uniform (getAge child) (getAge parent)) in
    simulate_side_branch t lambda mu;
    --let w = weight (logf 2) in
    weight (logf 2);
    ()
  in
  map _fbdy _rng;

  -- observe statement
  observe 0 (Poisson (mulf mu (subf (getAge parent) (getAge child)))); --85

  -- if ... is
  match child with Node { left = left, right = right, index = index } then
    observe 0 (Exponential (lambda));
    simulate_tree left tree lambda mu;
    simulate_tree right tree lambda mu;
    ()
  else ()
in


-- Translation of model function on line around 116
-- Instead of proper I/O I have hardcoded it io.mc
let lambda = assume (Gamma k_l t_l) in -- 117
let mu = assume (Gamma k_m t_m) in -- 118
match observation with Node { left = left, right = right, index = index } then -- 119
  simulate_tree left observation lambda mu;
  simulate_tree right observation lambda mu
else ());
{lambda, mu} -- line 123
