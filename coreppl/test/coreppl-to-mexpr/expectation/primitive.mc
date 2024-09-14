mexpr

-- NOTE(oerikss, 2024-09-14): We add this infer to prevent the backwards
-- compatible part of coreppl to wrap this program in a top-level infer.
infer (Default ()) (lam. ());

utest expectation (Gamma 2. 3.) with 6. in
utest expectation (Exponential 2.) with 0.5 in
utest expectation (Beta 2. 2.) with 0.5 in
utest expectation (Gaussian 1. 2.) with 1. in
utest expectation (Uniform 2. 3.) with 2.5 in

-- NOTE(oerikss, 2024-09-14): These expectations are supported in the runtime
-- but the type restriction `expectation : Dist Float -> Float` forbids them.
-- utest expectation (Poisson 2.) with 2. in
-- utest expectation (Binomial 0.5 2) with 1. in
-- utest expectation (Bernoulli 0.5) with 0.5 in

()
