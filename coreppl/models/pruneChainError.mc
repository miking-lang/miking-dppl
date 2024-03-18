mexpr
let f = lam x. let z =divf (int2float x) 4. in let y =divf (subf 1. z) 3. in [z,y,y,y] in

let a = prune (Categorical [0.25,0.25,0.25,0.25]) in
let p1 = f (pruned a) in
let b = prune (Categorical p1) in
let p2 = f (pruned b) in
observe (pruned b) (Categorical p2)

