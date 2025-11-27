mexpr

-- weight (addf (assume (Gaussian 0.0 1.0)) 2.0)

-- let a = 2.0 in
-- weight (addf (assume (Gaussian 0.0 1.0)) (addf a a))

-- let a = assume (Gaussian 0.0 1.0) in
-- weight (addf a a)

-- let f = lam x. addf 1.0 x in
-- addf (f 1.0) (f 2.0)

-- let f = lam x. addf (assume (Gaussian 1.0 0.0)) x in
-- addf (f 1.0) (f 2.0)

-- -- TODO(vipa, 2025-11-25): This doesn't generate what I want right
-- -- now, but there's an OPT comment in the idealized transformation
-- -- about it
-- let draw = lam x. assume (Gaussian x 1.0) in
-- get [draw 0.0, draw 1.0] (assume (Categorical [0.5, 0.5]))

-- let draw = lam x. assume (Gaussian x 1.0) in
-- let a = draw 0.0 in
-- let b = draw 1.0 in
-- switch assume (Categorical [0.5, 0.5])
-- case 0 then a
-- case 1 then b
-- end

-- let draw = lam x. assume (Gaussian x 1.0) in
-- let a = draw 0.0 in
-- let b = draw 1.0 in
-- let z = addf a b in
-- switch assume (Categorical [0.5, 0.5])
-- case 0 then a
-- case 1 then b
-- end

-- if assume (Bernoulli 0.5)
-- then [assume (Gaussian 0.0 1.0)]
-- else [2.0]


recursive let mul = lam acc. lam n. lam i.
  match i with 0 then acc else mul (addi acc n) n (subi i 1)
in mul 0 (addi 1 (assume (Categorical [0.25, 0.25, 0.25, 0.25]))) 4
