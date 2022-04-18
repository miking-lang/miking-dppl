------------------------------------------------------------------------------------
-- Constant-rate birth-death model (CRBD) model                                   --
-- Direct translation from TreePPL                                                --
------------------------------------------------------------------------------------

include "io.mc"

-- Tree type definition, lines 14 - 16
type Tree -- 14
con Leaf : { age : Float, index: Int } -> Tree -- 15
con Node : { left : Tree, right : Tree, age : Float, index: Int } -> Tree -- 16

-- Project the age from a tree
-- because of child.age and parent.age -- 78
let getAge = lam n: Tree. match n with Node r then r.age else
                 match n with Leaf r then r.age else
                 never

let getLeft_ = lam n: Tree. match n with Node r then r.age else
                 match n with Leaf r then r.age else
                 never

let getRight_ = lam n: Tree. match n with Node r then r.age else
                 match n with Leaf r then r.age else
                 never

-- "Hardcoding" input and output for now

-- 116

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
  if ltf current_time 0 then
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

-- Simulate tree function -- 64
recursive let simulate_tree: Tree -> Tree -> Float -> Float -> () =
  lam tree: Tree.
  lam parent: Tree.
  lam lambda: Float.
  lam mu: Float.
  
  let k = assume (Poisson (mulf lambda (subf (getAge_ parent) (getAge_ child)))) in
 
  -- loop lines 77 - 81 
  -- explicit l to u rather than a sequence (in the momemnt only static)
  let _l = 1 in
  let _u = k in
  recursive let _fbdy: Int -> () =
    lam idx: Int.
    if gti idx _u then () else
      let t = assume (Uniform (getAge child) (getAge parent)) in
      simulate_side_branch t lambda mu;
      weight (logf 2);
      _fbdy (addi idx 1)
  in
  _fbdy _l;

  -- observe statement
  observe 0 (Poisson (mulf mu (subf (getAge_ parent) (getAge_ child)))); --85

  -- if ... is
  if (match child with Node _ then true else false) then
    observe 0 (Exponential (lambda));
    simulate_tree (getLeft_ child) tree lambda mu;
    simulate_tree (getRight_ child) tree lambda mu;
    ()
  else ()
in


-- Translation of model function on line around 116
-- Instead of proper I/O I have hardcoded it io.mc
let lambda = assume (Gamma k_l t_l) in -- 117
let mu = assume (Gamma k_m t_m) in -- 118
(if (match observation with Node _ then true else false) then
  simulate_tree (getLeft_ observation) lambda mu;
  simulate_tree (getRight_ observation) lambda mu
else ());

{lambda = lambda, mu = mu} -- line 123
