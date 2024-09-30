include "math.mc"
include "ext/dist-ext.mc"

mexpr
-- NOTE(oerikss, 2024-09-19): A hack to prevent the backwards compability part
-- of coreppl to wrap the entire program in an infer.
(infer (Importance { particles = 1 }) (lam. ()));

let d : (Float -> Float) -> (Float -> Float) =
  lam f. lam x. let d = diff f x in d 1.
in

-- Differentiation involving non-differentiable intrinsics

utest d (lam x. mulf (int2float 2) x) 1. with 2. in
utest d (lam x. mulf (string2float "2.") x) 1. with 2. in

let notID = lam x. string2float (float2string x) in
utest d (lam x. mulf (notID 2.) x) 1. with 2. in
-- utest d notID 2. with 0. in -- should result in a runtime error

-- Differentiation involving externals

setSeed 1;

utest d (lam x. mulf (gaussianSample 1. 1.) x) 1. with 1.77896425246
  using eqfApprox 1e-5
in

-- utest d (lam x. mulf (gaussianSample x 1.) x) 1. with 0. using eqfApprox 1e-5 in -- should result in a runtime error


-- Differentiation involving expected values

utest d (lam x. mulf (expectation (Gaussian 2. 1.)) x) 1. with 2. in
-- utest d (lam x. mulf (expectation (Gaussian 2. x)) x) 1. with 2. in -- should result in a runtime error

()
