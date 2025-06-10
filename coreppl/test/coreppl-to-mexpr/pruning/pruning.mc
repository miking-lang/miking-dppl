-- Single observe --
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical params)

let modelNP = lam. 
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical params)

let modelPN = lam. 
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical params)

let modelPP = lam. 
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params)

let modelPPInCorrect = lam. 
  let x = prune (Categorical [0.4, 0.6]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params)

let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let incorrectPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none",prune=true}) modelPPInCorrect)
utest noPrunedResult with incorrectPrunedResult using (lam a. lam b. not (eqfApprox 1e-2 a b))

-- Double observe on x --
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical params);
  observe x (Categorical [0.9, 0.1])

let modelNP = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical params);
  observe x (Categorical [0.9, 0.1])

let modelPN = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical params);
  observe (pruned x) (Categorical [0.9, 0.1])

let modelPP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params);
  observe (pruned x) (Categorical [0.9, 0.1])

let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

-- Double observe on d --
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical params);
  observe d (Categorical [0.9, 0.1])

let modelNP = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])

let modelPN = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical params);
  observe d (Categorical [0.9, 0.1])

let modelPP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])


let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

-- Observe on both 'x' and 'd'
let modelNN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5])  in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe x (Categorical [0.9, 0.1]);
  observe x (Categorical params);
  observe d (Categorical [0.9, 0.1])
let modelNP = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe x (Categorical [0.9, 0.1]);
  observe x (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])
let modelPN = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned x) (Categorical params);
  observe d (Categorical [0.9, 0.1])
let modelPP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned x) (Categorical params);
  observe (pruned d) (Categorical [0.9, 0.1])
let modelPPSwap = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.5, 0.5]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  observe (pruned x) (Categorical params);
  observe (pruned x) (Categorical [0.9, 0.1]);
  observe (pruned d) (Categorical [0.9, 0.1])
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let incorrectPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none",prune=true}) modelPPSwap)
utest noPrunedResult with incorrectPrunedResult using (lam a. lam b. not (eqfApprox 1e-2 a b))

-- Pruning on single variable, single observe
let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f x in
  observe x (Categorical params)
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned x) in
  observe (pruned x) (Categorical params) 
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

-- Single variable, double observe
let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f x in
  observe x (Categorical params);
  observe x (Categorical [0.9, 0.1])
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned x) in
  observe (pruned x) (Categorical params);
  observe (pruned x) (Categorical [0.9, 0.1])
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let modelNN = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = assume (Categorical [0.7,0.3]) in
  let p = fun b in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = assume (Categorical [0.5,0.5]) in
  let p = fun a in
  cancel (observe b (Categorical [0.7,0.3]));
  observe b (Categorical p);
  observe e (Categorical p)
let modelNP = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = assume (Categorical [0.7,0.3]) in
  let p = fun b in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = prune (Categorical [0.5,0.5]) in
  let p = fun (pruned a) in
  cancel (observe b (Categorical [0.7,0.3]));
  observe b (Categorical p);
  observe e (Categorical p)
let modelPN = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = prune (Categorical [0.7,0.3]) in
  let p = fun (pruned b) in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = assume (Categorical [0.5,0.5]) in
  let p = fun a in
  cancel (observe (pruned b) (Categorical [0.7,0.3]));
  observe (pruned b) (Categorical p);
  observe e (Categorical p)
let modelPP = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = prune (Categorical [0.7,0.3]) in
  let p = fun (pruned b) in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = prune (Categorical [0.5,0.5]) in
  let p = fun (pruned a) in
  cancel (observe (pruned b) (Categorical [0.7,0.3]));
  observe (pruned b) (Categorical p);
  observe e (Categorical p)
let modelPPSwap = lam.
  let c = 1 in
  let d = 0 in
  let e = 1 in
  let fun = lam x. get [[0.4,0.6],[0.2,0.8]] x in
  let b = prune (Categorical [0.7,0.3]) in
  let p = fun (pruned b) in
  observe c (Categorical p);
  observe d (Categorical p);
  let a = prune (Categorical [0.5,0.5]) in
  let p = fun (pruned a) in
  observe (pruned b) (Categorical p);
  cancel (observe (pruned b) (Categorical [0.7,0.3]));
  observe e (Categorical p)
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelNN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelNP
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPN
              , infer (Importance {particles = 100000, cps = "none",prune=true}) modelPP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let incorrectPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none",prune=true}) modelPPSwap)
utest noPrunedResult with incorrectPrunedResult using (lam a. lam b. not (eqfApprox 1e-2 a b))


let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
  cancel (observe x (Categorical params));
  cancel (observe x (Categorical [0.9, 0.1]));
  cancel (observe x (Categorical [0.5, 0.5]))
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
  cancel (observe (pruned x) (Categorical params));
  cancel (observe (pruned x) (Categorical [0.9, 0.1]));
  cancel (observe (pruned x) (Categorical [0.5, 0.5]))
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts

let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let d = assume (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f d in
   (observe x (Categorical params));
   (observe 1 (Categorical params));
   (observe 0 (Categorical params))
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let d = prune (Categorical [0.4, 0.6]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned d) in
   (observe (pruned x) (Categorical params));
   (observe 1 (Categorical params));
   (observe 0 (Categorical params))
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts
-- Single variable, double observe
let modelN = lam.
  let x = assume (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f x in
  cancel (observe x (Categorical params));
  observe x (Categorical [0.9, 0.1])
let modelP = lam.
  let x = prune (Categorical [0.7, 0.3]) in
  let f = lam d. get [[0.9, 0.1], [0.2, 0.8]] d in
  let params = f (pruned x) in
  cancel (observe (pruned x) (Categorical params));
  observe (pruned x) (Categorical [0.9, 0.1])
let noPrunedResult = distEmpiricalNormConst (infer (Importance {particles = 100000, cps = "none"}) modelN)

let results = [ infer (Importance {particles = 100000, cps = "none",prune=true}) modelP
              ]

let normConsts = map distEmpiricalNormConst results
let x = iter (lam n. utest noPrunedResult with n using eqfApprox 1e-2 in ()) normConsts
mexpr
()


