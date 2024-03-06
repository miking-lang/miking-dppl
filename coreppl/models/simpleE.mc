mexpr
let fun = lam x. lam i. lam y. addi i (subi x y) in
let a = prune (Categorical [0.25,0.25,0.25,0.25]) in
let p = fun 4 (pruned a) 5 in


-- ANF
let fun = lam x. lam i. lam y. addi i (subi x y) in
let a = prune (Categorical [0.25,0.25,0.25,0.25]) in
let a1:Float = 4 in
let m1: = fun a1 in
let m2 = m1 (pruned a) in
let p = m2 5 in

-- how to figure this out?
Categorical (PruneFParam p)


