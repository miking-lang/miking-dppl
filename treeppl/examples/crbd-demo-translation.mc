------------------------------------------------------------------------------------
-- Constant-rate birth-death model (CRBD) model                                   --
-- Direct translation from TreePPL                                                --
------------------------------------------------------------------------------------

-- Tree type definition, lines 14 - 16
type Tree -- 14
con Leaf : { age : Float, index: Int } -> Tree -- 15
con Node : { left : Tree, right : Tree, age : Float, index: Int } -> Tree -- 16

include "io.mc" -- 116

mexpr -- Not sure where to put this?

-- Simulate tree function on line around 64
recursive simulate_tree: Tree -> Tree -> Float -> Float -> () {
    
}

-- Translation of model function on line around 116
-- Instead of proper I/O I have hardcoded it io.mc
let lambda = assume (Gamma k_l t_l) in -- 117
let mu = assume (Gamma k_m t_m) in -- 118
match observation with Node { left = left, right = right, index = index } then -- 119
  simulate_tree left observation lambda mu;
  simulate_tree right observation lambda mu
else ());
{lambda, mu} -- line 123
